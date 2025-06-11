close all

% Load data from the Excel file
data = readtable('data_once_month.xlsx');

% Extract only the first 27 rows for behavior data (starting from 4th column)
behaviors_clean = data{1:27, 4:end};

% Define behavior labels (adjust these names as appropriate)
behavior_names = {'Avoiding Contact', 'Avoiding Crowded Places', 'Frequent Hand Washing', 'Wearing Mask', ...
                  'Go to Work', 'Go to Gym', 'Visit Friend', 'Go to Cafe', ...
                  'Go to Doctor', 'Go to Church', 'Take Mass Transit', ...
                  'In Room with Someone', 'In Room with 5-10 People', ...
                  'In Room with 11-50 People', 'In Room with 50+ People'};

% Calculate pairwise correlation matrix for behaviors
correlation_matrix = corr(behaviors_clean, 'rows', 'complete');

% Round the correlation matrix to 2 decimal places
correlation_matrix = round(correlation_matrix, 2);

% Create a custom colormap: blue for negative, white for zero, red for positive
custom_colormap = [linspace(0, 1, 100)', linspace(0, 1, 100)', ones(100, 1);  % Blue to white
                   ones(100, 1), linspace(1, 0, 100)', linspace(1, 0, 100)']; % White to red

% Plot the correlation matrix as a heatmap
figure;
h = heatmap(behavior_names, behavior_names, correlation_matrix);
h.Title = 'Pairwise Correlation of Behaviors';
%h.XLabel = 'Behavior';
%h.YLabel = 'Behavior';
%set(struct(h).Axes.Title,'FontWeight','normal')

% Add custom title and axis labels with specified font sizes
%text(-1, 17, 'Pairwise Correlation of Behaviors', 'FontSize', 16, 'FontWeight', 'bold');  % Title
%text(-1, -1.5, 'Behavior', 'FontSize', 14);  % X-axis label
%text(-2.5, 9, 'Behavior', 'FontSize', 14, 'Rotation', 90);  % Y-axis label



% Apply the custom colormap and set color limits
colormap(custom_colormap);
h.ColorLimits = [-1, 1];
colorbar;

% Increase font size for better readability
h.FontSize = 12;

% Initialize array to store average correlations
num_behaviors = size(correlation_matrix, 1);
average_correlations = zeros(num_behaviors, 1);

% Calculate average correlation for each behavior, excluding self-correlation (diagonal)
for i = 1:num_behaviors
    % Exclude the diagonal element by setting it to NaN temporarily
    row = correlation_matrix(i, :);
    row(i) = NaN;
    average_correlations(i) = mean(abs(row), 'omitnan');  % Mean of absolute correlations
end

% Create a table with behavior names and their average correlations
average_correlation_table = table(behavior_names', average_correlations, ...
    'VariableNames', {'Behavior', 'AverageCorrelation'});

% Display the table
disp(average_correlation_table);

% Find the behavior with the maximum and minimum average correlation
[max_avg_corr, max_index] = max(average_correlations);
[min_avg_corr, min_index] = min(average_correlations);

fprintf('The behavior with the highest average correlation is "%s" with an average correlation of %.2f.\n', ...
    behavior_names{max_index}, max_avg_corr);
fprintf('The behavior with the lowest average correlation is "%s" with an average correlation of %.2f.\n', ...
    behavior_names{min_index}, min_avg_corr);

% Calculate and display the overall average correlation (excluding diagonal self-correlations)
correlation_matrix_no_diag = correlation_matrix;
for i = 1:num_behaviors
    correlation_matrix_no_diag(i, i) = NaN;
end
overall_average_correlation = mean(abs(correlation_matrix_no_diag(:)), 'omitnan');
fprintf('The overall average correlation across all behaviors is %.2f.\n', overall_average_correlation);
