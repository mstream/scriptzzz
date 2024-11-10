function checkEnvironmentalVariableProvided(value, name) {
  if (!value) {
    process.stderr.write(`"${name}" environmental variable not provided.\n`)
    process.exit(1)
  }
}

const isCiEnvVarName = "CI"
const firefoxBinaryPathEnvVarName = "FIREFOX_BINARY_PATH"
const geckoDriverBinaryPathEnvVarName = "GECKODRIVER_BINARY_PATH"

const isCi = process.env[isCiEnvVarName] == "true"
const firefoxBinaryPath = process.env[firefoxBinaryPathEnvVarName]
const geckoDriverBinaryPath = process.env[geckoDriverBinaryPathEnvVarName]

checkEnvironmentalVariableProvided(firefoxBinaryPath, firefoxBinaryPathEnvVarName)
checkEnvironmentalVariableProvided(geckoDriverBinaryPath, geckoDriverBinaryPathEnvVarName)

export const config = {
  specs: [
    './test/ui/specs/**/*.js'
  ],
  exclude: [],
  maxInstances: 10,
  capabilities: [{
    browserName: 'firefox',
    "moz:firefoxOptions": {
      // WORKAROUND: headless mode does not work for MacOS
      args: isCi ? ['--headless'] : [],
      binary: firefoxBinaryPath,
    },
    "wdio:geckodriverOptions": {
      binary: geckoDriverBinaryPath,
    },
  }],
  logLevel: 'info',
  bail: 0,
  waitforTimeout: 10_000,
  connectionRetryTimeout: 120_000,
  connectionRetryCount: 3,
  runner: 'local',
  services: ['visual'],
  framework: 'mocha',
  reporters: ['spec'],
  mochaOpts: {
    ui: 'bdd',
    timeout: 60_000
  },
}
