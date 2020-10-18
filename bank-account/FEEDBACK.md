# Feedback

@ostera was kind enough to give me some feedback on my solution and answer some of my questions, so here it goes.

### Question: How do people generally structure similar erlang programs? What are the responsibilities of a process?

The important part of deciding whether to split a computation into its own process, is to define how it is going to fail and what you want to do about it e.g, failure isolation is what makes `Erlang` so good at building large systems.
For example: if you spawn a `bank_account_process` and it dies, no harm no foul, your entire system will not crash. In fact, you'll learn mechanisms to restart things -- I like to call this part the "Necromancy" of Erlang.

The more you do on an individual process, the more chances that process has to fail because any of its steps will fail.
In this case it sounds like you'd be testing this code in a shell, calling `bank_account:create()` and receiving a pid that you then use in your deposit and your withdraw calls.

Yes, you can turn this into just a library, so that calling `bank_account:create()` actually returns the internal state of the process you're spawning now
so instead of returning a pid, it just returns an empty bank account `#bank_account{}` and that's the thing you will pass to your deposit and withdraw calls. This has 2 problems:

1. since all data is immutable, you can not concurrently update this bank account (multiple users depositing money or withdrawing it)
2. whoever holds the `#bank_account{}` value needs to figure out what to do if `bank_account:deposit crashes`.

In the current approach, these 2 are "solved":

1. once you have a Pid, you can send messages to it from anywhere in the entire system, so you can concurrently send sequences of deposit and withdraw messages
2. if that Pid crashes, the processes that were using it before are completely isolated from that crash
   sometimes one approach makes sense, sometimes the other does -- it is just another tool at your disposal to build your systems

### Question: Who transforms data into messages?

Since messages are just <b>erlang values</b>, this transformation doesn't really exist - but its a good idea to provide an api of functions that are acceptable messages that do all of the possible validation on them.

For example, the `deposit_amount` function does a check on the `Amount` to see if it is less than zero. You can prevent this way earlier, in the `deposit/2` function instead.

So if I call `deposit(Pid, 10)` I get an `ok` (just ok, we don't use {ok} unless we have another value packed with it) back, but if I call `deposit(Pid, -10)` I immediately get an `{error, invalid_amount}` before the message is even sent.

In this sense, you should think of "sending a message" just like "sending an http request", but a lot cheaper and faster. You normally want to do all the validation possible on the request data as early as possible: is it valid JSON, is the JSON object something I can parse into my internal representation, is that internal representation valid, ok we passed all this then the core logic runs.

The "server" also needs to do some checks that the "client" can't do since it doesn't have the full information, but at least at that point you know that all the requests that you are receiving are well formed.

For example, I can ask for a withdrawal and a valid withdrawal request may look lik `{pid(), non_negative_number()}`, so the server can count on that, and if the request doesn't look like that, we just crash, but if it does, then you want to check if there is enough money to withdraw and return an error if there isn't.

### Dialyzer

You can add Dialyzer specs for many things here, and if you split your `bank_account_process/1` function into a lot of tiny `handle_deposit_amount`, `handle_withdraw_amount`, etc, then you can add specs for those individually to make sure they work in isolation.

However, long atoms can have typos and dialyzer won't complain about all programs (but the one it does complain about it is right about).
Check out [caramel](https://github.com/AbstractMachinesLab/caramel) to get OCaml on the BEAM :ocaml: :erlang: -- build processes safely and then use the Erlang magic to run hundreds of millions of them.

### Question: layers

Question: If I am thinking about layers in a project with say a rest api, there you have controller, an api service that does the validation and calls some internal service, then the api service is what will actually unpack the result from the service and transform it into something that can be returned to the user, controller will just deal with Json-things. Is it possible to map this structure to an Erlang program and see which category processes falls into?

Answer: now this is tempting but sometimes not necessary!

For example, the "standard" http server in erlang, cowboy, will spawn a new process for each connection - this means that if your request process crashes, my request will not be affected by it.

But normally, in the lifecycle of a single request, you want to make sure that if anything goes wrong then the request process crashes -- so then you just have your request process doing all the work (using libraries, instead of sending messages to other processes). There's this tension in the design process, because having more processes means the failures will be more isolated, but it also means that can have concurrency scenarios that you didn't have before from a more pragmatic perspective, having everything done by a single process means that if there is a crash you have only one stacktrace to look at if you split the work in 3 sub processes so that

A sends message to B, C, and D
A waits for B, C, and D

then if B, C, or D crash they will have their own isolated stacktrace and you'll have to stitch these things together, so having one process per layer doesn't always work in your favor.

An example where you would want to have a separate process already started would be to use a database. So an HTTP request comes in, your Request process starts, and you don't want to connect to the database on every request, you want to reuse an existing connection, so you make sure you have a db process up and running, and you just send a query to it and wait for the response(this also means that this process can now receive multiple queries and either bach them, or spin new processes to handle each query individually, for example).
