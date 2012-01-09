-module (lib_stats).
-include_lib("records.hrl").
-export ([cumulative_moving_mean/2]).

%% Args (deliberately reflect return tuple):
%%  Cumulative_mean  : current running mean
%%  Num_samples     : number of samples so far
%%  NewSample       : new data observation to include in mean
%% returns {{mean, Cumulative_mean}, {samples, Num_samples}}
-spec cumulative_moving_mean({{mean, float()}, {samples, pos_integer()}}, number())
        -> {float(), pos_integer()}.

cumulative_moving_mean(#mean{}=CMean, New_sample) ->
    Cumulative_mean = CMean#mean.value,
    Num_samples = CMean#mean.samples,
    New_mean = ((Cumulative_mean * Num_samples) + New_sample) / (Num_samples + 1),
    CMean#mean{value = New_mean, samples = Num_samples + 1}.
