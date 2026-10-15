const assert = require('node:assert/strict');
const { test } = require('node:test');
const { EventEmitter } = require('node:events');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const Module = require('node:module');
const esbuild = require('esbuild');

const actionsCode = esbuild.buildSync({ entryPoints: ['src/desktop/actions.ts'], bundle: true,
    platform: 'node', format: 'cjs', external: ['vscode', 'undici'], write: false }).outputFiles[0].text;
const extensionCode = esbuild.buildSync({ entryPoints: ['src/desktop/extension.ts'], bundle: true,
    platform: 'node', format: 'cjs', external: ['vscode', 'undici', 'vscode-languageclient/node'], write: false }).outputFiles[0].text;

function load(code, mocks) {
    const module = new Module(__filename);
    module.require = name => name in mocks ? mocks[name] : require(name);
    module._compile(code, __filename);
    return module.exports;
}

function host() {
    const commands = new Map(), terminals = [], errors = [], invocations = [], sockets = [], clients = [];
    const config = { 'propanLsp.path': './bin/propan-lsp' };
    const entries = new Map();
    const disposable = { dispose() {} };
    const file = filename => ({ scheme: 'file', fsPath: filename });
    class Emitter {
        emitter = new EventEmitter();
        event = listener => { this.emitter.on('data', listener); return disposable; };
        fire(data) { this.emitter.emit('data', data); }
        dispose() { this.emitter.removeAllListeners(); }
    }
    const document = { uri: file('/workspace/current.propan'), isDirty: false, save: async () => true };
    const vscode = {
        Uri: { file }, EventEmitter: Emitter,
        Position: class { constructor(line, character) { Object.assign(this, { line, character }); } },
        Range: class { constructor(start, end) { Object.assign(this, { start, end }); } },
        Diagnostic: class { constructor(range, message, severity) { Object.assign(this, { range, message, severity }); } },
        DiagnosticSeverity: { Error: 0, Warning: 1, Information: 2 },
        languages: { createDiagnosticCollection: () => ({ ...disposable, clear: () => entries.clear(),
            set: values => values.forEach(([uri, value]) => entries.set(uri.fsPath, value)) }) },
        commands: { registerCommand: (name, callback) => { commands.set(name, callback); return disposable; } },
        workspace: {
            isTrusted: true, workspaceFolders: [{ uri: file('/workspace') }],
            getWorkspaceFolder: () => ({ uri: file('/workspace') }),
            getConfiguration: () => ({ get: (name, fallback) => config[name] ?? fallback }),
            openTextDocument: async uri => ({ ...document, uri }), saveAll: async () => true,
            onDidChangeConfiguration: callback => { vscode.changed = callback; return disposable; },
        },
        window: {
            activeTextEditor: { document }, showErrorMessage: message => errors.push(message),
            createTerminal: options => {
                const terminal = { ...disposable, options, output: '',
                    show() {
                        options.pty.onDidWrite(text => { terminal.output += text; });
                        options.pty.open();
                    },
                    done: new Promise(resolve => options.pty.onDidClose(code => {
                        terminal.exitCode = code;
                        resolve(code);
                    })),
                };
                terminals.push(terminal);
                return terminal;
            },
        },
    };
    class Socket {
        constructor(url) { this.url = url; sockets.push(this); queueMicrotask(() => this.onopen?.()); }
        send(packet) { this.packet = packet; }
        close() { this.closed = true; }
    }
    class Client {
        constructor(id, name, server) { this.server = server; clients.push(this); }
        async start() { if (config.failLsp) { throw new Error('missing LSP'); } }
        async stop() { this.stopped = true; }
        async dispose() { this.disposed = true; }
    }
    const mocks = { vscode, undici: { WebSocket: Socket }, 'vscode-languageclient/node': { LanguageClient: Client },
        'node:child_process': { execFile(command, args, options, callback) {
            invocations.push({ command, args, options });
            queueMicrotask(() => callback(config.assemblyError ?? null, Buffer.from([1, 2, 3]), Buffer.from(config.stderr ?? '')));
        } },
    };
    const context = { subscriptions: [] };
    return { vscode, config, commands, context, errors, entries, terminals, invocations, sockets, clients,
        actions: load(actionsCode, mocks), extension: load(extensionCode, mocks) };
}

test('diagnostics group imported files, handle colons, and convert UTF-8 columns to UTF-16', () => {
    const h = host();
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'propan-diagnostics-'));
    try {
        fs.writeFileSync(path.join(dir, 'unicode.propan'), 'BYTE "é😀", bad\n');
        const diagnostics = h.actions.parseDiagnostics(
            'unicode.propan:1:15: error: undefined symbol\n' +
            'lib:one.propan:4:2: warning: a warning\r\n' +
            'lib:one.propan:5:1: info: a note\n    1 | ignored excerpt\n', dir);
        assert.equal(diagnostics.length, 2);
        assert.equal(diagnostics[0][1][0].range.start.character, 11);
        assert.equal(diagnostics[1][0].fsPath, path.join(dir, 'lib:one.propan'));
        assert.deepEqual(diagnostics[1][1].map(d => d.severity), [1, 2]);
    } finally { fs.rmSync(dir, { recursive: true }); }
});

async function assertTerminalStaysOpen(terminal) {
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(terminal.exitCode, undefined, 'successful actions must leave the terminal open');
}

test('Check uses the clicked file, workspace cwd, sibling assembler and refreshes diagnostics', async () => {
    const h = host();
    h.actions.registerFileActions(h.context);
    h.config['propanLsp.path'] = './propan-lsp';
    h.config['propan.arguments'] = ['-I', 'inc'];
    h.config.stderr = 'import.propan:2:3: warning: test warning\n';
    await h.commands.get('propan.check')(h.vscode.Uri.file('/workspace/other.p2asm'));
    await assertTerminalStaysOpen(h.terminals[0]);
    assert.equal(h.invocations[0].command, '/workspace/propan');
    assert.deepEqual(h.invocations[0].args, ['-I', 'inc', '--format', 'none', '/workspace/other.p2asm']);
    assert.equal(h.invocations[0].options.cwd, '/workspace');
    assert.equal(h.entries.get('/workspace/import.propan')[0].message, 'test warning');
    assert.equal(h.sockets.length, 0);
    h.config.stderr = '';
    await h.commands.get('propan.check')();
    await assertTerminalStaysOpen(h.terminals[1]);
    assert.equal(h.entries.size, 0);
});

test('Run blocks failed assembly, preserves diagnostics and never uploads', async () => {
    const h = host();
    h.actions.registerFileActions(h.context);
    h.config.assemblyError = { code: 1 };
    h.config.stderr = 'current.propan:2:3: error: unknown mnemonic BAD\n';
    await h.commands.get('propan.run')();
    assert.equal(await h.terminals[0].done, 1);
    assert.equal(h.entries.get('/workspace/current.propan')[0].severity, 0);
    assert.equal(h.sockets.length, 0);
    assert.match(h.terminals[0].output, /Assembly failed/);
});

test('Run uploads a length-prefixed, aligned binary and streams split UTF-8 output', async () => {
    const h = host();
    h.actions.registerFileActions(h.context);
    h.config['p2aas.endpoint'] = 'ws://localhost:1234/run?baudrate=9600&token=abc';
    await h.commands.get('propan.run')();
    await new Promise(resolve => setImmediate(resolve));
    const socket = h.sockets[0];
    assert.deepEqual([...socket.packet], [4, 0, 0, 0, 1, 2, 3, 0]);
    assert.equal(socket.url.searchParams.get('baudrate'), '9600');
    assert.equal(socket.url.searchParams.get('timeout_ms'), '5000');
    assert.equal(socket.url.searchParams.get('token'), 'abc');
    socket.onmessage({ data: Uint8Array.from([0xc3]).buffer });
    socket.onmessage({ data: Uint8Array.from([0xa9, 10]).buffer });
    socket.onmessage({ data: 'done\n' });
    socket.onclose({ code: 1000 });
    await assertTerminalStaysOpen(h.terminals[0]);
    assert.match(h.terminals[0].output, /é\r\ndone\r\n/);
    assert.deepEqual(h.invocations[0].args, ['--format', 'flat', '-o', '-', '/workspace/current.propan']);
});

test('invalid endpoints/images, disconnects and cancellation are reported', async () => {
    const h = host(), image = Buffer.from([1]);
    const controller = new AbortController();
    await assert.rejects(h.actions.runImage(image, 'http://localhost', () => {}, controller.signal), /ws:\/\//);
    await assert.rejects(h.actions.runImage(Buffer.alloc(0), 'ws://localhost', () => {}, controller.signal), /nonempty/);
    await assert.rejects(h.actions.runImage(Buffer.alloc(512 * 1024 + 1), 'ws://localhost', () => {}, controller.signal), /512 KiB/);
    let running = h.actions.runImage(image, 'ws://localhost', () => {}, controller.signal);
    h.sockets[0].onclose({ code: 1008, reason: 'board busy' });
    await assert.rejects(running, /board busy/);
    running = h.actions.runImage(image, 'ws://localhost', () => {}, controller.signal);
    controller.abort();
    await assert.rejects(running, /Cancelled/);
    assert.equal(h.sockets[1].closed, true);
});

test('cancelled saves release the action lock; unsupported files and untrusted workspaces do not execute', async () => {
    const h = host();
    h.actions.registerFileActions(h.context);
    h.vscode.workspace.saveAll = async () => false;
    await h.commands.get('propan.check')();
    h.vscode.workspace.saveAll = async () => true;
    await h.commands.get('propan.check')();
    await assertTerminalStaysOpen(h.terminals[0]);
    await h.commands.get('propan.run')(h.vscode.Uri.file('/workspace/other.txt'));
    h.vscode.workspace.isTrusted = false;
    await h.commands.get('propan.run')();
    assert.equal(h.invocations.length, 1);
});

test('LSP restart rereads executable settings, and failed startup leaves file actions registered', async () => {
    const h = host();
    h.config.failLsp = true;
    await h.extension.activate(h.context);
    assert.equal(h.clients[0].server.command, '/workspace/bin/propan-lsp');
    assert.ok(h.commands.has('propan.check'));
    assert.match(h.errors[0], /missing LSP/);
    h.config.failLsp = false;
    h.config['propanLsp.path'] = '/tools/propan-lsp';
    h.config['propanLsp.arguments'] = ['--example'];
    await h.commands.get('propan.restartLsp')();
    assert.equal(h.clients[0].stopped, true);
    assert.equal(h.clients[0].disposed, true);
    assert.deepEqual(h.clients[1].server, { command: '/tools/propan-lsp', args: ['--example'] });
    h.vscode.changed({ affectsConfiguration: () => true });
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(h.clients.length, 3);
    await h.extension.deactivate();
    assert.equal(h.clients[2].stopped, true);
});
