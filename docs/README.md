# Introduction

UNDER DEVELOPMENT: JavaScript compiler for multithreading. Examine the top-level function in the JavaScript source code and output the function as multithreaded source code using Web Workers API.

## Motivation

Javascript is single threaded. Therefore, there is a problem that when you execute a process that takes a long time, other processes will stop. Currently, there is Web Workers API for multi-threading on the browser. However, the cost of fixing existing source code remains high. For the above reasons, I want to generate multithreaded source code automatically.
