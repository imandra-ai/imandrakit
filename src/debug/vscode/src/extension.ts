import * as vscode from 'vscode';
import { ChildProcess } from 'child_process';
import { WorkspaceFolder, DebugConfiguration, ProviderResult, CancellationToken } from 'vscode';

class DAPTracker implements vscode.DebugAdapterTracker {
  onWillStartSession?(): void { }
  onWillReceiveMessage?(message: vscode.DebugProtocolMessage): void {
    console.debug(`VSCODE2DAP: ${JSON.stringify(message)}`);
  }
  onDidSendMessage?(message: vscode.DebugProtocolMessage): void {
    console.debug(`DAP2VSCODE: ${JSON.stringify(message)}`);
  }
  onWillStopSession?(): void { }
  onError?(error: Error): void {
    console.error(`DAP ERROR: ${error}`);
  }
  onExit?(code: number | undefined, signal: string | undefined): void {
    console.debug(`DAP EXIT: ${code}/${signal}`);
  }
}

class DAPTrackerFactory implements vscode.DebugAdapterTrackerFactory {
  createDebugAdapterTracker(_: vscode.DebugSession): ProviderResult<vscode.DebugAdapterTracker> {
    return new DAPTracker();
  }
}

export function activate(context: vscode.ExtensionContext) {
  const provider = new ImandraxDebugConfigurationProvider();
  context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('imandrakit-debug', provider));

  let factory = new DebugAdapterServerDescriptorFactory();

  context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('imandrakit-debug', factory));

  if ('dispose' in factory) {
    context.subscriptions.push(factory);
  }

  vscode.debug.registerDebugAdapterTrackerFactory("*", new DAPTrackerFactory());
}

export function deactivate() { }

class ImandraxDebugConfigurationProvider implements vscode.DebugConfigurationProvider {
  resolveDebugConfiguration(folder: WorkspaceFolder | undefined, config: DebugConfiguration, token?: CancellationToken): ProviderResult<DebugConfiguration> {

    console.log("resolveDebugConfiguration");
    if (!config.type && !config.request && !config.name) {
      const editor = vscode.window.activeTextEditor;
      if (editor && editor.document.languageId === 'ocaml') {
        config.type = 'imandrakit-debug';
        config.name = 'Launch';
        config.request = 'launch';
        config.program = '${file}';
        config.env = '${env}';
        config.stopOnEntry = true;
      }
    }

    if (!config.program) {
      return vscode.window.showInformationMessage("Cannot find a program to debug").then(_ => {
        return undefined;
      });
    }

    return config;
  }
}

type SessionState = {
  child_process?: ChildProcess;
  port: number;
};

class DebugAdapterServerDescriptorFactory implements vscode.DebugAdapterDescriptorFactory {
  private session_states: Map<string, SessionState> = new Map();

  async createDebugAdapterDescriptor(session: vscode.DebugSession, executable: vscode.DebugAdapterExecutable | undefined): Promise<vscode.ProviderResult<vscode.DebugAdapterDescriptor>> {

    let state = this.session_states.get(session.id);

    if (state !== undefined) {
      state.child_process?.kill('SIGTERM');
      state.child_process = undefined;
      state.port = 0;
    }
    else {
      state = {
        child_process: undefined,
        port: 0
      };
    }

    let p = new Promise<[any, number]>((resolve, reject) => {
      let first = true;
      var spawn = require('child_process').spawn;
      let cp = spawn(session.configuration.program, session.configuration.args, {
        cwd: session.configuration.cwd,
        env: session.configuration.env
      });

      cp.stdout.setEncoding('utf8');
      cp.stdout.on('data', (data: string) => {
        if (first) {
          let n: number = +data;
          resolve([cp, n]);
        }
        else { console.log('' + data); }
      });
      cp.stderr.on('data', (data: string) => {
        console.log('' + data);
      });
      cp.on('exit', (code: number, signal: string) => {
        state.child_process = undefined;
      });
    });

    [state.child_process, state.port] = await p;

    this.session_states.set(session.id, state);

    return new vscode.DebugAdapterServer(state.port);
  }

  dispose() {
    for (const [_, state] of this.session_states) {
      if (state.child_process) {
        console.log("killing child process");
        if (state.child_process !== undefined) {
          state.child_process.kill('SIGKILL');
        }
        state.child_process = undefined;
        state.port = 0;
      }
      this.session_states.clear();
    }
  }
}
