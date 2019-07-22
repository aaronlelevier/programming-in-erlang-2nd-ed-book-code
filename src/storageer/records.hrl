%% todo: use `code:add_pathz` correctly so don't need to call `c("storageer/<name>") for each file here

%% an entity that stores units
%% todo: use `typer` to make name and units required arguments
-record(storageer, {name, units}).

%% use to track inventory, availability and so on
-record(units, {in_use=0, total=0}).