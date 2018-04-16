# Related Work

## Pi Calculus

Berger et al: The Two-Phase Commit Protocol in an Extended Pi-Calculus

Does not seem concerned with concurrency.

Uses pi-calculus enriched with various extensions to model questions of correctness of 2PC. E.g. they add a 'fail' and non-deterministically tag this on to every process in order to simulate dropping out of a network, that kind of thing.

They then prove various implementations bismilar to the simple version without the primitives - i.e. you still eventually observe a concensus. Or something a long those lines.

There is no program logic at work, I thiknk, only direct proofs of bismilarity via the equational theory of the Pi-calculi presented.

http://users.sussex.ac.uk/~mfb21/publications/express00/index.html

## Pro Verif

## A Distributed Pi-Calculus

What's this?

## Iris?

## Joins Library + HOCAP



