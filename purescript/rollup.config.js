import purs from "rollup-plugin-purs";
import commonjs from 'rollup-plugin-commonjs';
import nodeResolve from 'rollup-plugin-node-resolve';

export default {
  entry: "src/Main.purs",
  dest: "bundle.js",
  format: "iife",
  sourceMap: true,
  plugins: [
    nodeResolve({
      jsNext: true,
      main: true
    }),
    commonjs({
      include: 'node_modules/**'
    }),
    purs(),
  ]
};
