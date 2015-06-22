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


