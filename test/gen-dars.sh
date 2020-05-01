#!/bin/bash
# Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0
set -euxo pipefail

(cd bond-trading; daml build --output=../bond-trading.dar)
(cd damlc-tests; daml build --output=../damlc-tests.dar)
