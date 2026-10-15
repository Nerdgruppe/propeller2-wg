import * as vscode from 'vscode';
import { execFile } from 'node:child_process';
import { readFileSync } from 'node:fs';
import * as path from 'node:path';
import { StringDecoder } from 'node:string_decoder';
import { WebSocket } from 'undici';

// Bare names use PATH; relative paths use the selected file's workspace folder.
export function executable(value: string, resource?: vscode.Uri): string {
    if (path.isAbsolute(value) || !/[\\/]/.test(value)) {
        return value;
    }
    const folder = resource && vscode.workspace.getWorkspaceFolder(resource);
    return path.resolve(folder?.uri.fsPath ?? (resource ? path.dirname(resource.fsPath) : process.cwd()), value);
}

export function parseDiagnostics(text: string, cwd: string): [vscode.Uri, vscode.Diagnostic[]][] {
    const files = new Map<string, vscode.Diagnostic[]>();
    const sources = new Map<string, string[]>();
    for (const line of text.split(/\r?\n/)) {
        const match = /^(.*):(\d+):(\d+): (error|warning|info): (.*)$/.exec(line);
        if (!match) {
            continue;
        }
        const [, filename, row, column, level, message] = match;
        const uri = vscode.Uri.file(path.resolve(cwd, filename));
        if (!sources.has(uri.fsPath)) {
            try {
                sources.set(uri.fsPath, readFileSync(uri.fsPath, 'utf8').split(/\r?\n/));
            } catch {
                sources.set(uri.fsPath, []);
            }
        }
        const lineIndex = Math.max(0, Number(row) - 1);
        const byteIndex = Math.max(0, Number(column) - 1);
        const sourceLine = sources.get(uri.fsPath)?.[lineIndex];
        const character = sourceLine === undefined ? byteIndex : Buffer.from(sourceLine).subarray(0, byteIndex).toString('utf8').length;
        const position = new vscode.Position(lineIndex, character);
        const severity = level === 'error' ? vscode.DiagnosticSeverity.Error :
            level === 'warning' ? vscode.DiagnosticSeverity.Warning : vscode.DiagnosticSeverity.Information;
        const diagnostic = new vscode.Diagnostic(new vscode.Range(position, position), message, severity);
        diagnostic.source = 'propan';
        const entries = files.get(uri.fsPath) ?? [];
        entries.push(diagnostic);
        files.set(uri.fsPath, entries);
    }
    return [...files].map(([filename, entries]) => [vscode.Uri.file(filename), entries]);
}

export async function runImage(image: Buffer, endpoint: string, write: (text: string) => void, signal: AbortSignal): Promise<void> {
    const url = new URL(endpoint);
    if (url.protocol !== 'ws:' && url.protocol !== 'wss:') {
        throw new Error('propeller2.p2aas.endpoint must use ws:// or wss://.');
    }
    if (!url.searchParams.has('baudrate')) {
        url.searchParams.set('baudrate', '115200');
    }
    if (!url.searchParams.has('timeout_ms')) {
        url.searchParams.set('timeout_ms', '5000');
    }
    if (image.length === 0 || image.length > 512 * 1024) {
        throw new Error('P2AAS requires a nonempty image of at most 512 KiB.');
    }
    signal.throwIfAborted();
    const size = (image.length + 3) & ~3;
    const packet = Buffer.alloc(4 + size);
    packet.writeUInt32LE(size);
    image.copy(packet, 4);
    const socket = new WebSocket(url);
    socket.binaryType = 'arraybuffer';
    const decoder = new StringDecoder('utf8');
    await new Promise<void>((resolve, reject) => {
        const finish = (error?: Error) => {
            clearTimeout(timer);
            signal.removeEventListener('abort', abort);
            socket.onopen = socket.onmessage = socket.onerror = socket.onclose = null;
            socket.close();
            write(decoder.end());
            if (error) {
                reject(error);
            } else {
                resolve();
            }
        };
        const abort = () => finish(new Error('Cancelled.'));
        const timeout = Number(url.searchParams.get('timeout_ms'));
        const timer = setTimeout(() => finish(new Error('P2AAS connection timed out.')),
            (Number.isFinite(timeout) && timeout > 0 ? timeout : 5000) + 15000);
        signal.addEventListener('abort', abort, { once: true });
        socket.onopen = () => {
            socket.send(packet);
            write(`Uploaded ${size} bytes.\n`);
        };
        socket.onmessage = event => {
            write(typeof event.data === 'string' ? event.data : decoder.write(Buffer.from(event.data)));
        };
        socket.onerror = () => finish(new Error('P2AAS WebSocket connection failed. Check the endpoint and board availability.'));
        socket.onclose = event => finish(event.code === 1000 ? undefined :
            new Error(`P2AAS disconnected (${event.code})${event.reason ? ': ' + event.reason : ''}`));
    });
}

export function registerFileActions(context: vscode.ExtensionContext): void {
    const diagnostics = vscode.languages.createDiagnosticCollection('propan');
    context.subscriptions.push(diagnostics);
    // One current assembly owns the Problems entries, including imported files.
    let busy = false;
    for (const action of ['check', 'run'] as const) {
        context.subscriptions.push(vscode.commands.registerCommand(`propan.${action}`, async (uri?: vscode.Uri) => {
            uri ??= vscode.window.activeTextEditor?.document.uri;
            if (!uri || uri.scheme !== 'file' || !/\.(propan|p2asm)$/i.test(uri.fsPath)) {
                void vscode.window.showErrorMessage('Select a .propan or .p2asm file.');
                return;
            }
            if (!vscode.workspace.isTrusted || busy) {
                void vscode.window.showErrorMessage(busy ? 'A Propan action is already running.' : 'Trust this workspace to run Propan tools.');
                return;
            }
            busy = true;
            let terminalCreated = false;
            try {
                const document = await vscode.workspace.openTextDocument(uri);
                if (document.isDirty && !await document.save()) {
                    return;
                }
                // Imports may also be open and edited.
                if (!await vscode.workspace.saveAll(false)) {
                    return;
                }
                const resource = uri;
                const config = vscode.workspace.getConfiguration('propeller2', resource);
                const lsp = config.get<string>('propanLsp.path', 'propan-lsp');
                const defaultAssembler = /[\\/]/.test(lsp) ? path.join(path.dirname(executable(lsp, resource)), process.platform === 'win32' ? 'propan.exe' : 'propan') : 'propan';
                const command = executable(config.get<string>('propan.path') || defaultAssembler, resource);
                const cwd = vscode.workspace.getWorkspaceFolder(resource)?.uri.fsPath ?? path.dirname(resource.fsPath);
                const args = [...config.get<string[]>('propan.arguments', []), ...(action === 'run' ? ['--format', 'flat', '-o', '-'] : ['--format', 'none']), resource.fsPath];
                const writeEmitter = new vscode.EventEmitter<string>();
                const closeEmitter = new vscode.EventEmitter<number>();
                const controller = new AbortController();
                let started = false;
                const write = (text: string) => writeEmitter.fire(text.replace(/\r?\n/g, '\r\n'));
                const terminal = vscode.window.createTerminal({
                    name: `Propan ${action}: ${path.basename(resource.fsPath)}`,
                    isTransient: true,
                    pty: {
                        onDidWrite: writeEmitter.event,
                        onDidClose: closeEmitter.event,
                        open: () => {
                            started = true;
                            void (async () => {
                                write(`${action === 'run' ? 'Assembling' : 'Checking'} ${resource.fsPath}\n`);
                                diagnostics.clear();
                                const result = await new Promise<{ image: Buffer; stderr: string; failed: boolean }>((resolve, reject) => {
                                    execFile(command, args, { cwd, encoding: 'buffer', maxBuffer: 16 * 1024 * 1024, timeout: 30000, signal: controller.signal }, (error, stdout, stderr) => {
                                        if (error && typeof error.code !== 'number') {
                                            reject(error);
                                        } else {
                                            resolve({ image: stdout, stderr: stderr.toString('utf8'), failed: !!error });
                                        }
                                    });
                                });
                                controller.signal.throwIfAborted();
                                write(result.stderr);
                                diagnostics.set(parseDiagnostics(result.stderr, cwd));
                                if (result.failed) {
                                    throw new Error('Assembly failed. See Problems and the diagnostics above.');
                                }
                                write('Assembly succeeded.\n');
                                if (action === 'run') {
                                    await runImage(result.image, config.get<string>('p2aas.endpoint', 'ws://127.0.0.1:12880/'), write, controller.signal);
                                    write('\nRun finished.\n');
                                }
                                busy = false;
                            })().catch(error => {
                                busy = false;
                                write(`\n${String(error)}\n`);
                                closeEmitter.fire(1);
                            });
                        },
                        close: () => {
                            controller.abort();
                            if (!started) {
                                busy = false;
                            }
                            writeEmitter.dispose();
                            closeEmitter.dispose();
                        },
                    },
                });
                terminalCreated = true;
                context.subscriptions.push(terminal);
                terminal.show();
            } catch (error) {
                void vscode.window.showErrorMessage(`Propan ${action} failed: ${String(error)}`);
            } finally {
                if (!terminalCreated) {
                    busy = false;
                }
            }
        }));
    }
}
