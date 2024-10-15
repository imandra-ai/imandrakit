all: vsix

package-lock.json: package.json
	npm i

# see https://code.visualstudio.com/api/working-with-extensions/bundling-extension#using-esbuild
vsix: package-lock.json src/extension.ts
	npm x vsce package

distrib: vsix

.PHONY: all vsix distrib