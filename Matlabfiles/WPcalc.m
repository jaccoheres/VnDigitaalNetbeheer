%Some bookkeeping for the specific WP scenario's. Then calculate using a
%slightly modified port.m
%By Werner van Westering MSc.
%Start 18-06-2014

% %Load data
% clear all
% load('WPdata.mat')
% 
% %Define output
% indexlist = zeros(1,numel(PC6));

%% Put WP's on correct place (First time only)
% %Advanced cell mechanics: define an inline cell function
% cellfind = @(string)(@(cell_contents)(strcmp(string,cell_contents)));
% 
% %Find the index list
% for ii = 1:numel(PC6)
%     disp(ii)
%     
%     %Try to find the PC in output data
%     index = find(cellfun(cellfind(num2str(PC6{ii})),nHouses_PC));
%     
%     if isempty(index)
%         error('PC not found!')
%     end
%     
%     indexlist(ii) = index;
% end
% 
% %Generate results in port.m format and only calculate the 2030 scenario.
% nWPRen_Low = zeros(18,8822);
% nWPRen_Med = zeros(18,8822);
% nWPRen_High = zeros(18,8822);
% nWPRen_Max = zeros(18,8822);
% 
% nWPRen_Low(1:18,indexlist) = Minclust';
% nWPRen_Med(1:18,indexlist) = Medclust';
% nWPRen_High(1:18,indexlist) = Maxclust';
% nWPRen_Max(1:18,indexlist) = Extreme';
% save('WPinput.mat')

%% port.m starts here
%Quick port of an Excel model
%By Werner van Westering MSc.
%Start 04-06-2014
clear all
load('data.mat') %Load data
load('extradata.mat')
load('WPinput.mat')

clear PC6 Profiles

%Variable description
%nHouses : Number of houses
%nEV: Number of EV's for corresponding scenarios
%nPV: Same for PV panels
%nWPNB: Same for CHP in new houses
%nWPRen: Same for CHP in renovated houses
%User_profiles: Electricity use with respect to time
%Use_EV: Power consumption/production EV (same goes for PV and WP)

%% Calculations
%Bookkeeping
nsamples = numel(Time)/18; %Calculate the number of samples per year
%Rewrite for easy reference
% nEV{1} = nEV_Low;       nEV{2} = nEV_Med;       nEV{3} = nEV_High;
% nEV{4} = nEV_Max; %Now included in extradata.mat
nPV{1} = nPV_Low;       nPV{2} = nPV_Med;       nPV{3} = nPV_High;
nWPRen{1} = nWPRen_Low; nWPRen{2} = nWPRen_Med; nWPRen{3} = nWPRen_High; nWPRen{4} = nWPRen_Max;
dominantprofile(isnan(dominantprofile)) = ones(sum(isnan(dominantprofile)),1); %Fix NaN's out of PC areas without houses

%For loops are slow, but there is not enough memory for vectorization
%Calculate household use per
for ll = 1:4 %EV
for kk = 1:3 %PV
for jj = 1:4 %WPRen
for ii = 1:18 %Year
    %% Check if resultas already exist
    
    %Setup savestring
    if ll == 1
        lstr = '_LoW_EV_';
    elseif ll == 2
        lstr = '_Med_EV_';
    elseif ll == 3
        lstr = '_High_EV_';
    elseif ll == 4
        lstr = '_Max_EV_';
    end
    if kk == 1
        kstr = 'LoW_PV_';
    elseif kk == 2
        kstr = 'Med_PV_';
    else
        kstr = 'High_PV_';
    end
    if jj == 1
        jstr = 'LoW_WP_';
    elseif jj == 2
        jstr = 'Med_WP_';
    elseif jj == 3
        jstr = 'High_WP_';
    elseif jj == 4
        lstr = 'Max_WP_';
    end
    year = ii + 2012;
    
    filestr = ['minPC_WPclust' num2str(year) lstr kstr jstr '.csv'];
    
    %% Main calculations
    if exist(filestr, 'file') ~= 2 %Check if results already exist
        
    str = [num2str(ii+2012) ', ' num2str(ll) ', ' num2str(kk) ', ' num2str(jj)];
    disp(str)
    tic
    
    %Turned out to be slower
%     total_use = (nHouses(ii,:)'*User_profiles(:,3)')' + ...
%         (nEV{ll}(ii,:)'*EV_profile')' + ...
%         (nPV{kk}(ii,:)'*PV_profile')' + ...
%         (nWPRen{jj}(ii,:)'*WP_profile')'; 

    %Calculate the maximum use
    total_use = zeros(size(User_profiles,1),size(nHouses,2));
    total_use(:,dominantprofile==1) = (nHouses(ii,dominantprofile==1)'*User_profiles(:,1)')';          %Base load
    total_use(:,dominantprofile==2) = (nHouses(ii,dominantprofile==2)'*User_profiles(:,2)')';     
    total_use(:,dominantprofile==3) = (nHouses(ii,dominantprofile==3)'*User_profiles(:,3)')'; 
    total_use(:,dominantprofile==4) = (nHouses(ii,dominantprofile==4)'*User_profiles(:,4)')'; 
    total_use = (nEV{ll}(ii,:)'*EV_profile')' + total_use;      %EV Use
    total_use = (nPV{kk}(ii,:)'*PV_profile')' + total_use;      %PV Use
    total_use = (nWPRen{jj}(ii,:)'*WP_profile')' + total_use;   %nWPRen Use 
    
    %% Find timestamp with total maximum power use
    [~, time] = max(sum(total_use,2));
    
    %Also save the amount of power each type contributes
    base = (nHouses(ii,:)'.*User_profiles(time,dominantprofile)')';
    EV = (nEV{ll}(ii,:)'*EV_profile(time)')';
    PV = (nPV{kk}(ii,:)'*PV_profile(time)')';
    WP = (nWPRen{jj}(ii,:)'*WP_profile(time)')';
    
    maxuse = [total_use(time,:) ; base ; EV ; PV ; WP];
    %Divide by the total number of households to obtain the max
    %use/household
    maxuse = maxuse ./ [nHouses(ii,:);nHouses(ii,:);nHouses(ii,:);nHouses(ii,:);nHouses(ii,:)];
    
    %Save results
    filestr = ['max_WPclust' num2str(year) lstr kstr jstr num2str(time) '.csv'];
    csvwrite(filestr,maxuse);
    
    %Find timestamp with total minimum power use
    [~, time] = min(sum(total_use,2)); 
            
    %Also save the amount of power each type contributes
    base = (nHouses(ii,:)'.*User_profiles(time,dominantprofile)')';
    EV = (nEV{ll}(ii,:)'*EV_profile(time)')';
    PV = (nPV{kk}(ii,:)'*PV_profile(time)')';
    WP = (nWPRen{jj}(ii,:)'*WP_profile(time)')';
    
    minuse = [total_use(time,:) ; base ; EV ; PV ; WP];
    %Divide by the total number of households to obtain the max
    %use/household
    minuse = minuse ./ [nHouses(ii,:);nHouses(ii,:);nHouses(ii,:);nHouses(ii,:);nHouses(ii,:)];
    
    filestr = ['min_WPclust' num2str(year) lstr kstr jstr num2str(time) '.csv'];
    csvwrite(filestr,minuse);
    
    
    %% Find timestamp with maximum power use per PC
    [maximum, time] = max(total_use,[],1);
            
    %Also save the amount of power each type contributes
    base = zeros(1,size(nHouses,2));
    base(dominantprofile==1) = (nHouses(ii,dominantprofile==1).*User_profiles(time(dominantprofile==1),1)');
    base(dominantprofile==2) = (nHouses(ii,dominantprofile==2).*User_profiles(time(dominantprofile==2),2)');
    base(dominantprofile==3) = (nHouses(ii,dominantprofile==3).*User_profiles(time(dominantprofile==3),3)');
    base(dominantprofile==4) = (nHouses(ii,dominantprofile==4).*User_profiles(time(dominantprofile==4),4)');
    EV = (nEV{ll}(ii,:).*EV_profile(time)');
    PV = (nPV{kk}(ii,:).*PV_profile(time)');
    WP = (nWPRen{jj}(ii,:).*WP_profile(time)');
    
    maxuse = [maximum ; base ; EV ; PV ; WP];
    %Divide by the total number of households to obtain the max
    %use/household
    maxuse = maxuse ./ [nHouses(ii,:);nHouses(ii,:);nHouses(ii,:);nHouses(ii,:);nHouses(ii,:)];
    
    filestr = ['maxPC_WPclust' num2str(year) lstr kstr jstr '.csv'];
    csvwrite(filestr,[maxuse; time]);
    
    %Find timestamp with minimal power use per PC
    [minimum, time] = max(total_use,[],1);
           
    %Also save the amount of power each type contributes
    base = zeros(1,size(nHouses,2));
    base(dominantprofile==1) = (nHouses(ii,dominantprofile==1).*User_profiles(time(dominantprofile==1),1)');
    base(dominantprofile==2) = (nHouses(ii,dominantprofile==2).*User_profiles(time(dominantprofile==2),2)');
    base(dominantprofile==3) = (nHouses(ii,dominantprofile==3).*User_profiles(time(dominantprofile==3),3)');
    base(dominantprofile==4) = (nHouses(ii,dominantprofile==4).*User_profiles(time(dominantprofile==4),4)');
    EV = (nEV{ll}(ii,:).*EV_profile(time)');
    PV = (nPV{kk}(ii,:).*PV_profile(time)');
    WP = (nWPRen{jj}(ii,:).*WP_profile(time)');
    
    minuse = [minimum ; base ; EV ; PV ; WP];
    %Divide by the total number of households to obtain the max
    %use/household
    minuse = minuse ./ [nHouses(ii,:);nHouses(ii,:);nHouses(ii,:);nHouses(ii,:);nHouses(ii,:)];
    
    filestr = ['minPC_WPclust' num2str(year) lstr kstr jstr '.csv'];
    csvwrite(filestr,[minuse; time]);
    
    toc
    end
end
end
end
end
disp('Done!')