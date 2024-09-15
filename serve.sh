set -e

npm install 
spago install 
spago test --main Test.Scriptzzz.Main 
esbuild --bundle index.js --outfile=dist/index.js --servedir=dist
