#!/bin/bash

cabal run -- mathbounty-pab \
  --config testnet/pab-config.yml webserver \
  --passphrase pab123456789
