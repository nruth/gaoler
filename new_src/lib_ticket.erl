%%% Data wrapper module for bakery algorithm tickets
-module (lib_ticket).
-export ([ticket/2, number/1, resource/1]).
-record (ticket, {resource = undefined, number = undefined}).

%% give me a ticket
ticket(Resource, Number) -> 
    #ticket{resource=Resource, number=Number}.

%% give me the ticket's number
number(Ticket) -> 
    Ticket#ticket.number.

%% give me the ticket's resource
resource(Ticket) -> 
    Ticket#ticket.resource.
