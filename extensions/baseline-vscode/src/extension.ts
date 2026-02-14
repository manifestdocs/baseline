import * as path from "path";
import { workspace, ExtensionContext, window } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext): void {
  const config = workspace.getConfiguration("baseline");
  const serverPath = config.get<string>("serverPath", "blc");
  const serverArgs = config.get<string[]>("serverArgs", ["lsp"]);

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: serverArgs,
    options: {
      env: { ...process.env },
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "baseline" }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.bl"),
    },
    outputChannel: window.createOutputChannel("Baseline Language Server"),
  };

  client = new LanguageClient(
    "baseline",
    "Baseline Language Server",
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
