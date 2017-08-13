# meta-brollo

A bot to roll dice for a personal server.

Includes code to interface with mueval. However, it does this in a way that is
incompatible with resource limiting, so right now it only lets users use mueval
if they're on a (hard-coded) whitelist. I might fix this later by having it
call mueval through the shell instead of as a haskell library.
