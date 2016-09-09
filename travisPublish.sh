#!/usr/bin/env bash
set -e

if [ ${TRAVIS_SECURE_ENV_VARS} = "true" ]; then
  echo "Publishing snapshot..."
  # Assert that nightly is set to snapshot.
  grep "nightly.*SNAPSHOT" core/src/main/scala/org/scalafmt/Versions.scala
  # Save some useful information
  sbt publish
else
  echo "Skipping publish"
fi
