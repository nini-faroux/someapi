#!/bin/bash
docker volume rm some-api_pgdata
rm -rf pgdata
docker-compose -f docker-compose-local.yml up --build
