import { Launcher } from "@wdio/cli"
import { repoDir } from "../common.ts"
import { runRepoCommand } from "../common.ts"

async function getWdioBrowserDependencyPath(name: string): Promise<string> {
  return runRepoCommand(
    "nix",
    "eval",
    "--raw",
    `.#wdioBrowserDependencyPaths.${name}`
  );
}

export default async function({ host, port }): Promise<number | undefined> {
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
  const wdio = new Launcher(confFilePath, options)
  return wdio.run()
}
