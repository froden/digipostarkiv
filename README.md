# DPsync

Simple work in progress file sync client for files uploaded to Digipost

### Getting started

* install ghc	```brew install ghc```
* install haskell platform	```brew install haskell-platform```
* cabal configure
* cabal install
* Add credentials to sync.conf
* ```dist/build/dpsync/dpsync```

### Current status

1. Downloads all files marked as 'uploaded' from Digipost to local dir **overwrites any existing files with same name**
2. Uploads all new files from syncdir to Digipost

### TODO
* Use .sync to store state
* Full sync (download all + upload all) when .sync is missing
* Diff list from Digipost against .sync to determine:
  * Files deleted on server
  * Files deleted locally
  * New files on server
  * New files locally
