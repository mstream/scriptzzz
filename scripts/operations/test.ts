import * as path from "@std/path"
import { Launcher } from "@wdio/cli"
import { repoDir } from "../common.ts"
import { runRepoCommand } from "../common.ts"

export default async function({ host, port }): Promise<number | undefined> {
  const confFilePath = `${repoDir}/wdio.conf.js`
  const baseUrl = `http://${host}:${port}`
  const options = { baseUrl }
  const firefoxNixStorePath = await runRepoCommand("nix", "eval", "--raw", ".#firefox");
  const geckodriverNixStorePath = await runRepoCommand("nix", "eval", "--raw", ".#geckodriver");

  const firefoxBinaryPath = path.join(
    firefoxNixStorePath,
    "Applications",
    "Firefox.app",
    "Contents",
    "MacOS",
    "firefox"
  )

  const geckodriverBinaryPath = path.join(
    geckodriverNixStorePath,
    "bin",
    "geckodriver"
  )

  Deno.env.set("FIREFOX_BINARY_PATH", firefoxBinaryPath)
  Deno.env.set("GECKODRIVER_BINARY_PATH", geckodriverBinaryPath)

  const wdio = new Launcher(confFilePath, options)
  return wdio.run()
}
