#!/bin/bash

cabal run -- mathbounty-pab \
  --config testnet/pab-config.yml migrate
