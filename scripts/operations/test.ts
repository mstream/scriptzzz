import { Launcher } from "@wdio/cli"
import { repoDir } from "../common.ts"
import { runRepoCommand } from "../common.ts"

type Options = {
  host: string,
  port: number,
}

async function getWdioBrowserDependencyPath(name: string): Promise<string> {
  return await runRepoCommand(
    "nix",
    "eval",
    "--raw",
    `.#wdioBrowserDependencyPaths.${name}`
  );
}

export default async function({ host, port }: Options): Promise<number | undefined> {
  const confFilePath = `${repoDir}/wdio.conf.js`
  const baseUrl = `http://${host}:${port}`
  const options = { baseUrl }
  Deno.env.set(
    "FIREFOX_BINARY_PATH",
    await getWdioBrowserDependencyPath("firefox")
  )
  Deno.env.set(
    "GECKODRIVER_BINARY_PATH",
    await getWdioBrowserDependencyPath("geckodriver")
  )
  await runRepoCommand("deno", "check", "scripts")
  await runRepoCommand("deno", "lint", "scripts")
  await runRepoCommand("spago", "test")
  const wdio = new Launcher(confFilePath, options)
  return wdio.run()
}
