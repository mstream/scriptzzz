import * as path from "@std/path"
import * as esbuild from "esbuild"

class CommandError extends Error {
  constructor(commandInfo: CommandInfo, code: number | undefined) {
    super(`'${commandInfo.show()}' command has returned ${code} exit code`)
  }
}

class CommandInfo {
  args: Array<string>
  executable: string

  constructor(executable: string, args: Array<string>) {
    this.args = args
    this.executable = executable
  }

  show() {
    return [this.executable, ...this.args].join(" ")
  }
}

const scriptDir = path.resolve(path.dirname(path.fromFileUrl(import.meta.url)))

export const repoDir = path.join(scriptDir, "..")

export const esbuildCommonBuildOptions: esbuild.BuildOptions = {
  absWorkingDir: repoDir,
  bundle: true,
  entryPoints: ["index.js"],
  logLimit: 0,
  outfile: "dist/index.js",
  platform: "browser",
}

const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();

let currentRepoCommandInfo: CommandInfo | null = null

export async function runRepoCommand(executable: string, ...args: Array<string>): Promise<string> {
  if (currentRepoCommandInfo) {
    throw Error(`Repo commands cannot run simultenously. Current command: ${currentRepoCommandInfo.show()} `)
  }

  currentRepoCommandInfo = new CommandInfo(executable, args)

  const commandOptions: Deno.CommandOptions = {
    args,
    cwd: repoDir,
    stderr: "piped",
    stdout: "piped",
  }

  const command = new Deno.Command(executable, commandOptions)
  console.debug(`==> ${currentRepoCommandInfo.show()}`)
  const process = command.spawn()

  const output = await process.output()

  await Deno.stdout.write(output.stdout)
  await Deno.stdout.write(textEncoder.encode("\n"))

  await Deno.stderr.write(output.stderr)
  await Deno.stderr.write(textEncoder.encode("\n"))

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
      await Deno.stderr.write(textEncoder.encode(`${error.message}\n`))
    } else if (error instanceof Error) {
      await Deno.stderr.write(textEncoder.encode(`${error.stack}\n`))
    } else {
      await Deno.stderr.write(textEncoder.encode("Unknown error has happended.\n"))
    }
    Deno.exit(1)
  }
  Deno.exit(0)
}


