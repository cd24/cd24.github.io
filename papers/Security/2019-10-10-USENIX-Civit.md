---
title: Civet
pdf: papers/files/civit-usenix-2020.pdf
authors: Chia-Che Tsai, Jeongsok Son, Bhushan Jain, John McAvey, Raluca Ada Popa, Donald E. Porter
conference: USENIX Security
conferencepage: https://www.usenix.org/conference/usenixsecurity20/presentation/tsai
year: 2020
---

Hardware enclaves are designed to execute small pieces ofsensitive code or to operate on sensitive data, in isolation fromlarger, less trusted systems. Partitioning a large, legacy appli-cation requires significant effort. Partitioning an applicationwritten in a managed language, such as Java, is more challeng-ing because of mutable language characteristics, extensivecode reachability in class libraries, and the inevitability ofusing a heavyweight runtime.Civet is a framework for partitioning Java applications intoenclaves.  Civet reduces the number of lines of code in theenclave  and uses  language-level defenses,  including  deeptype checks and dynamic taint-tracking, to harden the enclaveinterface. Civet also contributes a partitioned Java runtime de-sign, including a garbage collection design optimized for thepeculiarities of enclaves. Civet is efficient for data-intensiveworkloads;  partitioning a Hadoop mapper reduces the en-clave overhead from 10×to 16–22% without taint-trackingor 70–80% with taint-tracking.
