# lambda_prolog
A implementation of lambda expressions in prolog.

This implementation aims to enable one to parse lambda expressions in the form of usual rules, into callable predicates.

This is achived by calling the predicate: make_lambda/2. The first parameter is the expression, in the form of 
Args:-Rules,
where Args is a list of variables, and Rules is a rule or conjunction of rules to be executed.
The second parameter then is the name of the predicate that you may call to execute the expression.

In the background this predicate registeres more predicates into the knowledge base, which is why it is advisable to clear them regularly.
This can be done by calling lambda_flush, or if only one specific predicate should be removed, one can first make it using make_lambda/3, where the first two parameters are as seen before, but the third is a reference to the added rule, so that it can easily be removed by calling clear_lambda/1 with this reference.

If one does not want to use this slightly odd way to handle these added predicates, one can also use lambda_map, which is equivalent to a maplist call, just that the first parameter has to be a lambda expression.


This implementation can be compared with the very commonly used library yall, which similarly implements lambda expressions.
The advantage of yall is that it is easier to get started using it.
This implementation can be much faster on average, since the lambda functions are designed to be used many times after each other, which really is the case  most of the time anyway.
Another difference between the two is that in yall, one needs to specifically denote free variables in their lambda expressions, to let them carry over to it. This is not neccessary in this implementation. They do not need to be denoted, and theoretically infinite free variables are possible.
