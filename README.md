# Clearnexus - Chrome Extension
## Set up
Clone the [repository][repository] with git:

```
$ git clone git@github.com:stackbuilders/clearnexus-chrome-extension.git
```

Add dependencies:

```
$ npm install
$ bower install
```

## Compile
Remember to run your Haskell application with the data types before compile the
PureScript code. Compile the project and compress it with `pulp`:

```
$ pulp browserify --to extension/clearnexus.js
```

## Run
Open your Google Chrome web browser and go to extensions.
Enable `Developer mode` and choose the option `Load unpacked extension...`.
Open the directory: `clearnexus-chrome-extension/extension/`. And use the extension
in a Gmail session.

[repository]: https://github.com/stackbuilders/clearnexus-chrome-extension
