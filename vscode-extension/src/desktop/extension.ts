import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';
import { registerFileActions, executable } from './actions';

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    registerFileActions(context);
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ language: 'propan', scheme: 'file' }],
        outputChannelName: 'Propan Language Server',
    };
    let restarting = Promise.resolve();
    const restart = () => {
        restarting = restarting.then(async () => {
            await client?.stop();
            await client?.dispose();
            const resource = vscode.window.activeTextEditor?.document.uri ?? vscode.workspace.workspaceFolders?.[0]?.uri;
            const config = vscode.workspace.getConfiguration('propeller2', resource);
            const serverOptions: ServerOptions = {
                command: executable(config.get<string>('propanLsp.path', 'propan-lsp'), resource),
                args: config.get<string[]>('propanLsp.arguments', []),
            };
            client = new LanguageClient('propan', 'Propan Language Server', serverOptions, clientOptions);
            await client.start();
        }).catch(error => {
            void vscode.window.showErrorMessage(`Failed to start Propan LSP: ${String(error)}`);
        });
        return restarting;
    };
    context.subscriptions.push(
        vscode.commands.registerCommand('propan.restartLsp', restart),
        vscode.workspace.onDidChangeConfiguration(event => {
            if (event.affectsConfiguration('propeller2.propanLsp')) {
                void restart();
            }
        }),
    );
    await restart();
}

export async function deactivate(): Promise<void> {
    await client?.stop();
    await client?.dispose();
    client = undefined;
}
