#!/bin/bash
stack build
stack exec -- some-api-exe -d
redoc-cli bundle -o docs/index.html swagger-docs/api.json
