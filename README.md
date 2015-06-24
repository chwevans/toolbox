# toolbox
Minimal erlang system metrics collection

## Examples
* Measure an mfa:
```
toolbox:measure(M, F, A)
```
* Measure an anonymous function:
```
toolbox:measure(fun() -> 3 + 2 end)
```

* Trace a process to get periodic updates
```
toolbox:trace(Name, Pid)
```

* Trace a registered process to get periodic updates
```
toolbox:trace(RegisteredName)
```

* Stop tracing a process
```
toolbox:untrace(Name)
```

## Configuration
* Metrics function (metrics_fun): Module and function that represents a 2 arity function taking the name and relevant statistics in the form of a map:
```-spec log(atom(), map()) -> ok.```

* Metrics to collect (metrics): A list of metrics to collect, valid options include reductions and message_queue_len, or any numerical statistic from process_info
* Enable node statistics (enable_node_stats): A boolean that declares whether to send node statistics in the form of 
```#{absolutes => ListOfAbsoluteStatistics, incrementals => ListOfStatisticsThatMakeSenseOverTime}```

