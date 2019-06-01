---
title: Effective Capturing
date: 2018-02-02
categories: Software, Design
---

"I can't test it because it's effectful". I hear this phrase uttered around the corridors of every company I've worked at. This phrase comes in many flavors, but at its heart the issue is in effects. Many advocates of testing tend to use mocks, or test doubles, to verify that code is working as expected. In this article I am going to argue against mocks. Mocking has a place in the world, where it is not possible to test otherwise, but I have found that the mere existence of a mocking framework can be destructive to code quality. 

Before we begin, we should figure out *why* we want to test. While there are many good reasons to test during development, I have found the most salient to be that testing makes writing good code easier. I have found that development can often become a cycle of making a small change, rebuilding the application, launching the application, checking to see if it works. This is fine, but can become very slow if your project is large or has lots of cases to verify. It may seem, at times, that this is the only hope for validaing UIs without subjecting yourself you UI Automation frameworks that are both fragile and slow. This pattern is not unique to UI, the same concerns can be seen in networking mocks, and file system mocks.

Mocking can take several forms; two of the most popular are Dependency Injection (DI) and method swizzling. This post will only cover these topics briefly, further reading on them will be included below. 

### Method Swizzling

Swizzling is a technique by

### Dependency Injection
