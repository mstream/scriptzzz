import * as path from "@std/path"

class CommandError extends Error {
  constructor(commandInfo: CommandInfo, code: number | undefined) {
    super(`'${commandInfo.show()}' command has returned ${code} exit code`)
  }
}

class CommandInfo {
  args: Array<string>
  executable: string

  constructor(executable: string, ...args: Array<string>) {
    this.args = args
    this.executable = executable
  }

  show() {
    return [this.executable, ...this.args].join(" ")
  }
}

const scriptDir = path.resolve(path.dirname(path.fromFileUrl(import.meta.url)))

export const repoDir = path.join(scriptDir, "..")

const textDecoder = new TextDecoder('utf-8');

let currentRepoCommandInfo: CommandInfo | null = null

export async function runRepoCommand(executable: string, ...args: Array<string>): Promise<string> {
  if (currentRepoCommandInfo) {
    throw Error(`Repo commands cannot run simultenously. Current command: ${currentRepoCommandInfo.show()} `)
  }
  currentRepoCommandInfo = new CommandInfo(executable, args)
  const commandOptions = {
    args,
    cwd: repoDir,
    stderr: "piped",
    stdout: "piped",
  }
  const command = new Deno.Command(executable, commandOptions)
  const process = command.spawn()

  const output = await process.output()

  await Deno.stderr.write(output.stderr)
  await Deno.stdout.write(output.stdout)

  const { code, success } = await process.status

  if (!success) {
    throw new CommandError(currentRepoCommandInfo, code)
  }

  currentRepoCommandInfo = null

  return textDecoder.decode(output.stdout)
}

export async function runScript(script: () => Promise<unknown>): Promise<void> {
  try {
    await script()
  } catch (error) {
    if (error instanceof CommandError) {
      const encoder = new TextEncoder();
      const data = encoder.encode(error.message)
      await Deno.stderr.write(data)
    } else {
      throw error
    }
  }
}


