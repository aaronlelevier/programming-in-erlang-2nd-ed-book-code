%% todo: use `code:add_pathz` correctly so don't need to call `c("storageer/<name>") for each file here

%% use to track inventory, availability and so on
-record(units, {in_use=0, total=0}).

%% geo location - latitude and longitude
-record(latlon, {lat=0.0, lon=0.0}).

-record(location, {units = #units{}, latlon=#latlon{}}).

%% an entity that stores units
%% todo: use `typer` to make name and units required arguments
%% units available at this cost
-record(storageer, {name="", email="", address="", cost=0, units=#units{}}).

%% request for storage
-record(request, {start="", stop="", cost=0, phone="", email=""}).

%% made the request for storage
-record(user, {name="", phone="", email=""}).

%% message sent between two processes
-record(message, {text="", subject="", datetime=""}).
