import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    const serverOptions: ServerOptions = {
        command: '/home/felix/projects/nerdgruppe/propeller2-wg/zig-out/bin/propan-lsp',
        args: [],
    };
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ language: 'propan' }],
        outputChannelName: 'Propan Language Server',
    };
    client = new LanguageClient('propan', 'Propan Language Server', serverOptions, clientOptions);
    const languageClient = client;
    context.subscriptions.push(client, vscode.commands.registerCommand('propan.restartLsp', async () => {
        try {
            await languageClient.stop();
            await languageClient.start();
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to restart Propan LSP: ${String(error)}`);
        }
    }));
    await client.start();
}

export async function deactivate(): Promise<void> {
    await client?.stop();
    client = undefined;
}
