import type { Config } from "stylelint";

export default {
  ignoreFiles: ["result/**/*", "_cache/**/*", "_site/**/*"],
  extends: ["stylelint-config-standard", "stylelint-config-clean-order", "@double-great/stylelint-a11y/strict"],
  plugins: ["stylelint-browser-compat", "stylelint-declaration-block-no-ignored-properties"],
  rules: {
    "plugin/browser-compat": true,
    "plugin/declaration-block-no-ignored-properties": true,
  },
} as const satisfies Config;
