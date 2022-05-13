#!/bin/bash

target=$1
amount=$2
echo "make a bounty of target $1 for amount $2"

source ./env.sh

curl -X 'POST' \
  'http://localhost:9080/api/contract/activate' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '
{
  "caWallet": {
    "getWalletId": "'"$WALLETID"'"
    },
 "caID": {
 "contents": {
      "bAmount": 3000000,
      "bTarget": 9
    },
    "tag": "Bounty"
  }
}
'
