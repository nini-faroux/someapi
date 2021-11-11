#!/bin/bash
stack build
stack exec -- some-api-exe -d
redoc-cli bundle -o static/index.html swagger-docs/api.json
