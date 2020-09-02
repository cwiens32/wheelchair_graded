% Code to process data for paper (first section is from Ian's)
% Created - 2020 July 7

%% initialize variables and read in list of data
close all;clear;clc; dbstop if error
cross_t = @(u,v) cell2mat(arrayfun(@(ii){cross(u(ii,:).',v(ii,:).').'},1:size(u,1)).');
dot_t = @(u,v) cell2mat(arrayfun(@(ii){dot(u(ii,:).',v(ii,:).').'},1:size(u,1)).');

% Compile MAT files for each subject's pushes

% Select the first FORCE data file:
[filename1,PathName1] = uigetfile('*.MAT','Select MAT file');
% Move directory to that folder
cd(PathName1)

% Get a list of all files in the folder with the desired file name pattern.
filePattern = fullfile(PathName1, '*_short.MAT'); % Change to whatever pattern you need.
% create structure with list of full file names
matfiles = dir(filePattern);

% create table for subject ordering
subOrd = [19, 20, 29, 12, 11, 28, 10, NaN, 15, NaN, 27, 4, NaN, NaN, 5,...
    NaN, 7, NaN, 6, 23, 3, 2, 17, 9, 14, 8, 30, 24, 1, 13, 25, 26, 22,...
    16, 18, 21];

% initialize table
tableOut = table();
cnt1 = 1;

%% loop through each mat file
for k = 1: length(matfiles)
    
    %% from Ian's code
    clear Subject_Number Description_Number Cycle_Number Trial_Number
    close all
    baseFileName = matfiles(k).name;
    fullFileName = fullfile(PathName1, baseFileName);%read in specific force file
    %   fprintf(1, 'Now reading %s\n', fullFileName);
    
    %Load data by reading file
    cycle_data = load(fullFileName);
    %Extract Trial Number from File name
    fullFileName;
    Trial_Number = cycle_data.trialNumber;
    Subject_Number = cycle_data.subjectNumber;
    Description_Number = cycle_data.descriptionNumber;
    Cycle_Number = cycle_data.push_cycle;    
    
    RF_mag = cycle_data.RF_mag;
    Velocity_cycle = cycle_data.Velocity_cycle;
    elbow_angle = cycle_data.elbow_angle;
    Shoulder_NJM_mag = cycle_data.Shoulder_NJM_mag;
    Shoulder_NJM_imp_cycle = cycle_data.Shoulder_NJM_imp_cycle;
    Elbow_NJM_imp_cycle = cycle_data.Elbow_NJM_imp_cycle;
    WheelCenterXYZ = cycle_data.WheelCenterXYZ;
    WC_2_shoulder = [cycle_data.ShoulderXYZ(:,1:3)] - [cycle_data.WheelCenterXYZ(:,1:3)];%Vector from WC to Shoulder
    x_axis = [ones(length(WC_2_shoulder(:,1)),1) zeros(length(WC_2_shoulder(:,1)),1)];
    Torso_ang = acosd(dot(WC_2_shoulder(:,1:2).',x_axis.').'./(sqrt(sum(WC_2_shoulder(:,1:2).^2,2))));
    RF_angleForearm = acos(dot_t(cycle_data.Reaction_Force(:,1:2),(-cycle_data.ForearmLongVector(:,1:2)))./(sqrt(sum(cycle_data.Reaction_Force(:,1:2).^2,2)).*sqrt(sum(((-cycle_data.ForearmLongVector(:,1:2))).^2,2)))).*sign(cross_t(cycle_data.Reaction_Force(:,:),(-cycle_data.ForearmLongVector(:,:)))*[0 0 1].')*180/pi;
    
    
    %% set parameters
    dt = 1/240;
    
    %% Crop to MZ > +5Nm
    Mwheel = cycle_data.Mz;
    start = (find(Mwheel>5, 1, 'first'));
    stop = (find(Mwheel>5, 1, 'last'));
    
    %% process data..(cropped from Ian's code)

    RF_angleForearmCROP = RF_angleForearm(start:stop);

    RF_mag_CROP = cycle_data.RF_mag(start:stop);

    elbow_ang_CROP= cycle_data.elbow_angle(start:stop);
    clear elbow_angle_temp elb_vel
    elbow_angle_temp = cycle_data.elbow_angle(start:stop);
    for ppp = 2:length(elbow_angle_temp)
        elb_vel(ppp-1) = (elbow_angle_temp(ppp)-elbow_angle_temp(ppp-1))/dt;
    end
    elb_vel_CROP = elb_vel;
    torso_ang_CROP = Torso_ang(start:stop);
    clear torso_angle_temp torso_vel
    torso_angle_temp = Torso_ang(start:stop);
    for pppp = 2:length(torso_angle_temp)
        torso_vel(pppp-1) = (torso_angle_temp(pppp)-torso_angle_temp(pppp-1))/dt;
    end
    torso_vel_CROP = torso_vel;
    % peak reaction force during elbow extension
    [~, inddRF] = max( RF_mag_CROP );
    % save shoulder NJM magnitude during crop (using Mz)
    shoulderNJMmag_crop = Shoulder_NJM_mag(start:stop);
    
    %% save angle data
    save(fullFileName, 'shoulderNJMmag_crop', 'RF_angleForearmCROP',...
        'RF_mag', 'RF_mag_CROP',...
        'elbow_ang_CROP', 'elb_vel_CROP',...
        'torso_ang_CROP', 'torso_vel_CROP', '-append')

    %% cropped to elbow extension
    % use this to find instant of elbow extension
    [~,indd] = min( elbow_ang_CROP );
    % crop elbow velocity to push duration
    elb_vel_PUSH = elb_vel_CROP(indd:end);
    % crop elbow angle to push duration
    elb_ang_PUSH = elbow_ang_CROP(indd:end);
    % crop torso angle to push duration
    torso_ang_PUSH = torso_ang_CROP(indd:end);
    % find max elbow velocity
    [~,indEMV] = max( elb_vel_PUSH );
    % peak reaction force during elbow extension
    [~, ang_inddRF] = max( RF_mag_CROP(indd:end) );
    
    %% add data to table
    % USING CROPPED DATA FROM Mz > +5Nm
    % store subject number
    tableOut.Subject(cnt1,1) = Subject_Number;
    % store session number
    tableOut.Session(cnt1,1) = Description_Number;
    % store cycle number
    tableOut.CycleNumber(cnt1,1) = Cycle_Number;
    % store trial number
    tableOut.TrialNumber(cnt1,1) = str2double(Trial_Number);
    % store Mz crop start index
    tableOut.CropInd_Start(cnt1,1) = start;
    % store Mz crop stop index
    tableOut.CropInd_Stop(cnt1,1) = stop;

    % mean velocity
    tableOut.velocityMean(cnt1,1) = mean(Velocity_cycle(start:stop));
    % Elbow extension duration
    tableOut.push_duration(cnt1,1) = sum((stop-start))*dt;
    % Resultant Shoulder NJM Impulse
    tableOut.Shoulder_NJMmag_Impulse(cnt1,1) = sum(Shoulder_NJM_mag(start:stop))*dt;
    % store maximum elbow angular velocity
    tableOut.ElbowAngVelMax(cnt1,1) = max(elb_vel_CROP);
    % store elbow angle when elbow angular velocity is 0
    tableOut.ElbowAngAtZeroElbVel(cnt1,1) = elbow_ang_CROP(indd);
    % store elbow angle when max elbow angular velocity occurs
    tableOut.ElbowAngAtMaxElbVel(cnt1,1) = elbow_ang_CROP(indEMV);
    % store torso angle at start of elbow extension
    tableOut.TorsoAngleatElbowExt(cnt1,1) = torso_ang_CROP(indd);
    % store torso angle at max force applied to wheel
    tableOut.TorsoAngleatMaxForceonWheel(cnt1,1) = torso_ang_CROP(ang_inddRF);
    % reaction force angle relative to forearm at peak RF
    tableOut.RF_angleFA_PeakRF(cnt1,1) = RF_angleForearmCROP(inddRF);
    % store start of elbow extension index
    tableOut.Start_Elb_Ext_Ind(cnt1,1) = indd;
    % store peak elbow angular velocity index
    tableOut.RF_Elb_Ext_AngVel_Ind(cnt1,1) = indEMV;
    % store peak reaction force index
    tableOut.RF_peak_Ind(cnt1,1) = inddRF;
    % iterate counter
    cnt1 = cnt1 + 1;
    
end

%% write out table
writetable(tableOut, 'DOD_graded_results_CW_200831.xlsx')