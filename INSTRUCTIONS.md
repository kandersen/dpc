# Instructions

The commands in this file can be copy/pasted into the shell (`$ ...`) or GHCi repl (`> ...`), as appropriate.

## Setup & Build

To build with `stack`, first make sure you have the latest version:
```
$ stack upgrade
```
The project can be then compiled with
```
$ stack install
```
## Running The Simulation Interface

Once built, to load all the core libraries and examples with following command and press enter
```
$ stack repl
```
Then run the simulator of the corresponding distributed application as follows:
```
> runGUI DPC.Examples.AppName.initNetwork
```
where `AppName` is, e.g., `Calculator.Calculator` or `DistributedLocking`. Use digits `1-N` to choose the next system move. Exit the interface with `<ESC>`.

## Running the Dynamic Protocol Adherence Verifier

The pure interpretation of the implementation language can be used to produce execution traces.

Here we will quickly look at a simple instance of the calculator example: 1 server offering a `sum` operator, and two clients adding up to sets of numbers. The initial _execution_ configuration can be seen on line 156 of `src/DPC/Examples/Calculator/Calculator.hs`, and implementations are `addClient` and `addServer`.


Run e.g. 
```
> take 10 $ runPure (DPC.Examples.Calculator.Calculator.simpleConf :: ImplNetwork (DiSeL DPC.Examples.Calculator.Calculator.S) Int)
```
to get the 10 first steps of the fair execution of the initial configuration initConf.

We can "pretty print" the trace with the following:

```
> mapM_ (putStrLn . show . fst) $ take 55 $ runPure (DPC.Examples.Calculator.Calculator.simpleConf :: ImplNetwork (DiSeL DPC.Examples.Calculator.Calculator.S) Int)
```
we know it's finished as we can see the repeated `ServerAction[RPC "compute" <function> <function>]:InternalAction[0]` of the spinning receive loop on the server (node 0)..

To check the safety of the trace, run `checkTrace` on the actions in the trace, supplying an initial spec configuration of _protocol states_ for each node:
```
> DPC.Interpretations.Pure.checkTrace initStates . fmap fst $ take 60 $ runPure (DPC.Examples.Calculator.Calculator.simpleConf :: ImplNetwork (DiSeL DPC.Examples.Calculator.Calculator.S) Int)
```

A return of `Right ()` indicates success.

Now, try and change the `sum` in line 72 of Calculator.hs to `product`, or some other non-`sum` function. Reload and run again.

```
> :r
> DPC.Interpretations.Pure.checkTrace initStates . fmap fst $ take 60 $ runPure (DPC.Examples.Calculator.Calculator.simpleConf :: ImplNetwork (DiSeL DPC.Examples.Calculator.Calculator.S) Int)
```

An exception is thrown as the server did not follow the protocol as specified.
