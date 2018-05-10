---
title: A new look
date: 2018-05-09
---

# Hello world!

Today I am rolling out the new version of my personal site. I spent quite some time looking at various toolstacks and technologies. This post will provide some insight into my choice for my site base: [Hakyll](http://jaspervdj.be/hakyll).

As with all problems, the first and hardest part is figuring out what is should do. When I built the first version of my site, I placed aesthetic above performance. This was fine while I was accessing my site from my site from my home internet, but I foolishly attempted to share the site with one of my colleagues over 3G cellular - it didn't go well. So I set out to redesign the site with performance and aesthetics in mind. I wanted a site which was both fast and easy to read.

To this end, I decided to emulate a reader mode as closely as possible. Reader modes have become common place because websites are *too busy*. Since my hosting is GitHub Pages, I am also limited to client-only systems.

Lastly, whatever system my page is built in should be extensible. However, simplicity should not be given at the cost of configuration or extension. Whatever system I use should expose a way for me to write my own features.

I limited myself to static site generators for simplicity. When I first started looking into them I found the usual suspects: [Jekyll](https://jekyllrb.com) and [Hugo](https://gohugo.io). Jekyll offered nice integrations with GitHub Pages, but I found that I couldn't customize the output beyond some templating. It really lived up to its name as a blogging software. After comparing the two, the main difference appeared to be how fast it could generate the site. Lastly, a friend of mine recommended Hakyll. Hakyll was slower than the other two, but essentially provides a library for you to build your own static site builder.

Despite the performance hit, the ease of configuration and customization offered by Hakyll won me over and so. Here we are, now I just need to think some more about themes 🤔