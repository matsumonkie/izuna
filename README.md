# izuna

> Enhances Haskell code review for Github

Izuna is brings a richer github interface by showing type annotations directly in your browser.

## Requirements

As of today, the izuna plugin is only available for Chrome and your haskell project needs to be using GHC 8.10.1

## How do I use it?

To use it, enable the github action [izuna-action](https://github.com/matsumonkie/izuna-action/) for your project.
You will also need to add the izuna plugin in chrome by going to `chrome://extensions/` and clicking `load unpacked` (then select the `chrome-extension` folder)

## How does it work?

Izuna makes use of [.hi extended](https://gitlab.haskell.org/ghc/ghc/-/wikis/hie-files) (AKA **hie files**) to recover information about your code. Your project information is then displayed by a plugin in your browser.

A more detailed worklow is:
1. Every time you push a commit, a github action will upload the hie files to a server
2. The server will then processed the hie files
3. When you visit a pull request from your browser, information about this PR (if any available) will be fetched from the server and displayed in your browser thanks to a plugin.

## Features & Roadmap

✅: available<br/>
🔧: building<br/>


| available | feature                | description                                               |
|-----------|------------------------|-----------------------------------------------------------|
| ✅        | Type annotation        | Show type annotation for your haskell code                |
| ✅        | Unified diff view mode | Works correctly for unified diff view mode                |
| ✅        | Chrome support         |                                                           |
| 🔧        | Firefox support        |                                                           |
| 🔧        | Split diff view mode   |                                                           |
| 🔧        | GHC 8.10.2 support     | only GHC 8.10.1 is available atm                          |
| 🔧        | Security               | Make sure private repo are only accessible by their owner |

## Caveats

- Izuna is as of today (december 2020) a first draft and might fail in some scenario.
- There is no authentication/authorization present at the moment. Any individual that has the tuple **owner/repository/commitId** for your project will be able to access your code. Private repo should use Izuna at their own risks.

## How to build

### Github Action

Please go to the [izuna-action](https://github.com/matsumonkie/izuna-action/) repo for more information.

### izuna-Builder/izuna-server

izuna-builder is the core of the project. Its goals is to receive a hie files tar archive from the github action and extract it.
Then it needs to parse the hie files and recover any useful information.

Build with:
```bash
stack build izuna-builder --stack-yaml=stack-8.10.1.yaml
```

izuna-server is a simple server that returns the processed hie files for the plugin.

Build with:
```bash
stack build izuna-server --stack-yaml=stack-8.10.1.yaml
```

## Inspirations

Izuna was (more than) inspired by:
- [Haskell-code-explorer](https://github.com/alexwl/haskell-code-explorer) by Alexwl
- [Haddock](https://github.com/haskell/haddock/)

Kudos to:
- [weeder](https://github.com/ocharles/weeder/) by Ocharles
- [stan](https://github.com/kowainik/stan) by Kowainik
Which helps me understand better how Hie files work!
