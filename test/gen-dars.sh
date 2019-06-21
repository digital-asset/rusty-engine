#!/bin/bash
# Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0
set -euxo pipefail

export DAML_SDK_VERSION=0.13.5

daml damlc package --target=1.dev bond-trading/Main.daml bond-trading
daml damlc package --target=1.dev damlc-tests/All.daml damlc-tests
