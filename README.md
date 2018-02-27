# Network Simulation

## Building and Running

### Setup and build

To build with `stack`, first make sure you have the latest version:
```
stack upgrade
```
The project can be then compiled with
```
stack install
```
### Running simulations

Once built run 
```
stack repl
```

Then run the simulator of the corresponding distributed application as follows:
```
runGUI NetSim.AppName.initNetwork
```
where `AppName` is, e.g., `BatchCalculator` or `DistributedLocking`. Use digits `1-N` to choose the next system move.


