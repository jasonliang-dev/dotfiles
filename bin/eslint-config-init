#!/usr/bin/env sh

npx install-peerdeps --dev eslint-config-airbnb
npm install --save-dev eslint-config-prettier

echo '{
  "env": {
    "browser": true
  },
  "extends": ["airbnb", "prettier"],
  "rules": {
    "import/extensions": 0
  }
}' > ./.eslintrc

echo '{
  "singleQuote": true,
  "trailingComma": "all"
}' > ./.prettierrc
