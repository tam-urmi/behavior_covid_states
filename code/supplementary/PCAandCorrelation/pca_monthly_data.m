clc;
clear all;
close all;

data = importdata('dataonceamonth.mat');         % interpolated survey data
%data = importdata('PCAsurveyNOinter.mat') % survey data (not modified)
income = data(:,1);
datawecare = data(:,1:15);

datawecare = datawecare(1:26, :); %only extract 26 rows for our data

%without masking (on interpolated survey data) masking is 4th entry
%datawecare1 = data(:,1:3);
%2 = data(:,5:15);
%datawecare = [datawecare1 datawecare2]

%without masking (on interpolated survey data) masking is 11th entry
%datawecare1 = data(:,1:10);
%datawecare2 = data(:,12:15);
%datawecare = [datawecare1 datawecare2] 

correlation_of_data = corrcoef(datawecare); 

[coeff, score, latent, tsquared, explained,mu] = pca(datawecare);
%contour(coeff)
explained
coeffPC1 = coeff(:,1);
 [~, maxFeatureIndex] = max(abs(coeffPC1));

 
% Step 4: Determine the number of components that explain 95% variance
cumulativeVariance = cumsum(explained);
numComponents = find(cumulativeVariance >= 95, 1);

% Display the result
fprintf('Number of principal components explaining 95%% variance: %d\n', numComponents);

% Optional: Plot cumulative explained variance
ax = gca;
ax.FontSize = 104;

%tiledlayout(1,2)
%nexttile
plot(cumulativeVariance,LineWidth=3);
xlabel('Number of Principal Components',FontSize=24);
ylabel('Cumulative Explained Variance (%)',FontSize=24);
%title('PCA Explained Variance',FontSize=24);
xticks([1,2,3,5,10,15])
ax.XAxis.FontSize = 16;
ax.YAxis.FontSize = 16;

%nexttile
bar(explained);


grid on;

%% to determine the % contribution of the PC1

 figure(2)
 t = tiledlayout(1, 2, 'TileSpacing', 'compact', 'Padding', 'compact');
 nexttile
 
 contributionsquared = coeffPC1.^2;
 totalcontribution = sum(contributionsquared);
 contributionPERCENT = contributionsquared/totalcontribution*100
 c = bar(contributionPERCENT);

 c = bar(coeffPC1)
 



set(gca, 'XTickLabel',{'Avoid contact with other people','Avoid public or crowed places',...
    'Frequently hand washing','Wearing a face mask when outside of your home',...
    'Go to work','Go to gym','Go visit a friend','Go to cafe, bar, or resturant',...
    'Go to a doctor or visit a hospital','Go to church or another place of worship',...
    'Take mass transit (e.g. subway, bus, or train)','Been in a room with someone outside of a household in the past 24 hours',...
    'Been in a room with 5-10 people outside of household',...
    'Been in a room with 11-50 people outside of household',...
    'Been in a room with over 50 people outside of household'
    })
c.FaceColor = 'flat';

% loop that colors first four bars as orange
for i = 1:4
        c.CData(i,:) = [1 0.5 0];  % [R G B] values for orange
end

% loop that colors the remaining as green
for i = 5:15
        % Set the color of the rest of the bars to green
        c.CData(i,:) = [0 0.7 0];  % [R G B] values for green
end

ax = gca;
%ax.XAxis.FontSize = 12;
ax.YAxis.FontSize = 12;

ylabel('Principal component coefficients (loadings)',FontSize=20);
%ylabel('Loading',FontSize=20);
 xlabel('Risk-averting and risk-seeking behaviors',FontSize=20);
 title('(a)','Interpreter','latex',FontSize=25);


%% contribution towards the second component
%figure(3)
nexttile
coeffPC2 = coeff(:,2);
 [~, maxFeatureIndex] = max(abs(coeffPC2));

 contributionsquared = coeffPC2.^2;
 totalcontribution = sum(contributionsquared);
 contributionPERCENT = contributionsquared/totalcontribution*100
 c = bar(contributionPERCENT)

 c = bar(coeffPC2)


 set(gca, 'XTickLabel',{'Avoid contact with other people','Avoid public or crowed places',...
    'Frequently hand washing','Wearing a face mask when outside of your home',...
    'Go to work','Go to gym','Go visit a friend','Go to cafe, bar, or resturant',...
    'Go to a doctor or visit a hospital','Go to church or another place of worship',...
    'Take mass transit (e.g. subway, bus, or train)','Been in a room with someone outside of a household in the past 24 hours',...
    'Been in a room with 5-10 people outside of household',...
    'Been in a room with 11-50 people outside of household',...
    'Been in a room with over 50 people outside of household'
    })
c.FaceColor = 'flat';

% loop that colors first four bars as orange
for i = 1:4
        c.CData(i,:) = [1 0.5 0];  % [R G B] values for orange
end

% loop that colors the remaining as green
for i = 5:15
        % Set the color of the rest of the bars to green
        c.CData(i,:) = [0 0.7 0];  % [R G B] values for green
end

ax = gca;
%ax.XAxis.FontSize = 12;
ax.YAxis.FontSize = 12;

ylabel('Principal component coefficients (loadings)',FontSize=20);
 title('(b)','Interpreter','latex',FontSize=25);
%ylabel('Loading',FontSize=20);

 xlabel('Risk-averting and risk-seeking behaviors',FontSize=20);

 %% plot PCs
figure(4)
 PC1 = score(:, 1);
PC2 = score(:, 2);
PC1';
PC2';
plot(1:1:length(PC1),PC1,'r',LineWidth=4)
hold on
plot(1:1:length(PC1),PC2,'g',LineWidth=4)

%%
PC1tilt = PC1'
PC2tilt = PC2'
%%
maskingdata = data(:,4);
avoidcontact = data(:,1);
gotowork = data(:,5);

%plot(1:1:length(maskingdata),maskingdata,'b',LineWidth=4)
%plot(1:1:length(maskingdata),avoidcontact,'m',LineWidth=4)
%plot(1:1:length(maskingdata),gotowork,'k',LineWidth=4)






% Eigenvectors (coeff) corresponding to the first variable
W1 = coeff(1, 1); % Coefficient for PC1
W2 = coeff(1, 2); % Coefficient for PC2

% Calculate the approximation of the first variable
avoiding_contact_regen = PC1 * W1 + PC2 * W2;

avoiding_contact_regen = PC1 * W1 + PC2 * W2 + mu(1);

avoiding_contact_regen = PC1 * W1 + mu(1);


%avoiding_contact_regen = PC1 * W1 + PC2 * W2 + score(:, 3) * coeff(1,3) + score(:, 4) * coeff(1,4)

%plot(1:1:length(maskingdata),avoiding_contact_regen,'*-',LineWidth=4)

%legend('PC1','PC2','masking data','avoid contact', 'gotowork',fontsize=18)
%legend('First principal component (PC1)','Second principal component (PC2)','avoid contact data', '$0.43 PC1 - 0.30 PC2 + 35.63$','interpreter','latex',fontsize=18)

% Set the x-tick positions
xticks([1, 13, 25]);

% Set the corresponding x-tick labels
xticklabels({'4/30/2020', '4/30/2020', '4/30/2022'});

     ax = gca;
        ax.XAxis.FontSize = 13;
        ax.YAxis.FontSize = 13;
        hold off;


legend('Eigen behavior 1','Eigen behavior 2','interpreter','latex',fontsize=18)
ylabel('Principal Component Value','interpreter','latex',fontsize=20)
ylabel('Scores','interpreter','latex',fontsize=20)
xlabel('Time','interpreter','latex',fontsize=20)

%%
% Min-Max normalization for PC1
PC1_min = min(PC1);
PC1_max = max(PC1);
PC1_normalized = (PC1 - PC1_min) / (PC1_max - PC1_min);

% Min-Max normalization for PC2
PC2_min = min(PC2);
PC2_max = max(PC2);
PC2_normalized = (PC2 - PC2_min) / (PC2_max - PC2_min);

figure(999)
plot(1:1:length(PC1_normalized),PC1_normalized,'r',LineWidth=4)
hold on
plot(1:1:length(PC2_normalized),PC2_normalized,'g',LineWidth=4)

legend('Eigen behavior 1','Eigen behavior 2','interpreter','latex',fontsize=18)
ylabel('Principal Component Value','interpreter','latex',fontsize=20)
ylabel('Normalized adherence','interpreter','latex',fontsize=20)
xlabel('Time','interpreter','latex',fontsize=20)
%%


%%
% figure(10)
% plot(PC1, PC2)


%%