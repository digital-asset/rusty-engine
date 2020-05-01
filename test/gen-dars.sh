#!/bin/bash
# Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0
set -euxo pipefail

DAMLS="bond-trading collect-authority damlc-tests"

for DAML in $DAMLS; do
  (cd $DAML; daml build --output=../$DAML.dar)
done
