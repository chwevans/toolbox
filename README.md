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

## Configuration
* Metrics function (metrics_fun): 2 arity function that takes the name and relevant statistics in the form of a map:
```-spec log(atom(), map()) -> ok.```

* Metrics to collect (metrics): A list of metrics to collect, valid options include reductions and message_queue_len, or any numerical statistic from process_info

