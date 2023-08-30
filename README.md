[![Build Status](https://travis-ci.org/stackbuilders/clearnexus-chrome-extension.svg?branch=master)](https://travis-ci.org/stackbuilders/clearnexus-chrome-extension)
[![No Maintenance Intended](http://unmaintained.tech/badge.svg)](http://unmaintained.tech/)

> **⚠️ Warning:** This library has been deprecated and is no longer maintained. It will not receive any further security patches, features, or bug fixes and is preserved here at GitHub for archival purposes. If you want to use it, we suggest forking the repository and auditing the codebase before use. For more information, contact us at info@stackbuilders.com.

# DEPRECATED - Clearnexus - Chrome Extension

## Set Up

Clone the [repository][repository] with git:

```
$ git git@github.com:stackbuilders/clearnexus-chrome-extension.git
```

Install dependencies:

```
$ npm install
$ bower install
```


## Build extension

### Init configuration

Go to `/src/Config.purs` and change the `url` attribute of the config records according to your development,
staging (testing) or production servers. For example:

```
staging :: Config
staging = Config {
  url: <"YOUR STAGING SERVER URL HERE">
  }
```

Go to `/src/Main.purs` and change the name of the `environment` variable according to the environment in which you
want to build the extension. There are only three possible values, namely, `production`, `development`, `staging`,
with default in production. Take into account that this variable will determine which config record in `Config.purs`
will be picked to build the extension:

```
environment :: String
environment = <"production or development or staging">
```

Modify the `extension/manifest.json` file permissions if necessary. For example, it may be that in your development
environment you want to run the extension with a tuneling service like [ngrok](https://ngrok.com/). Thus, you have
to give permissions to the ngrok-url in the following way:

```
{
 ...,

 "permissions": [
	"activeTab",
	"tabs",
	"storage",
	"*://staging.clearnex.us/*",
	"*://ca639050.ngrok.io/*" --> EXAMPLE NGROK URL
	]
}
```
Make sure that the clearnexus server `url` you provide is forwarded against `https`. Otherwise, you may have
problems when the extension executes the ajax requests.

### Build

To build the extension for testing in development environment:

```
$ pulp browserify --to extension/clearnexus.js
```
To package the extension for production:

```
$ gulp build
```
You can find the extension in `dist/clearnexus.zip`


## Run

### Development

Open your Google Chrome web browser and go to extensions. Enable `Developer mode` and choose the option
`Load unpacked extension...`. Load the directory: `clearnexus-chrome-extension/extension`. And use the extension in a Gmail session.

### Production
Open your Google Chrome web browser and go to extensions. Drag and drop `dist/clearnexus.zip` file to install the extension. And use the extension in a Gmail session.


## Running tests

Set this environment variable (you can find sample values in `access_tokens` table):

```
CLEARNEXUS_USER_TEST_TOKEN
```
Run `pulp test`

[repository]: https://github.com/stackbuilders/clearnexus-chrome-extension

---
## License

MIT, see [the LICENSE file](LICENSE).

---
<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>  
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)
