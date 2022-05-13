#!/bin/bash

echo "solution"

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
      "proposed_solution": 3
    },
    "tag": "Solution"
  }
}
'
