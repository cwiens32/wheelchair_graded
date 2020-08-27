%% exportdata.m
% Exports digitized and net joint moment data from the '_ALL.mat' files
% that save all the data generated by the MatLab code Ian created.


%%
% gather all files ending in '*_short.mat'
cd('../data/data_mat_files')
files = dir('*_short.mat');

% loop through each file and output specific data
for cnt = 1:length(files)
    % load data
    load(files(cnt).name,...
        'WristXYZ', 'ElbowXYZ', 'ShoulderXYZ',... % digitized locations
        'wristF', 'elbowF', 'shoulderF',... % net joint forces
        'wristNJM',... % wrist net joint moment
        'elbowIa', 'elbowFwR', 'elbowFeR', 'elbowNJM',... % variables for elbow net joint moment calculations
        'shoulderIa', 'shoulderFeR', 'shoulderFsR', 'shoulderNJM',... % variables for shoulder net joint moment calculations
        'RF_angleForearmCROP', 'RF_mag_CROP',... % variables for reaction force angle and magnitude
        'elbow_ang_CROP', 'elb_vel_CROP', 'torso_ang_CROP', 'torso_vel_CROP'); % variables for reaction force
    
    % create table for first sheet
    tab1_out = array2table([WristXYZ, ElbowXYZ, ShoulderXYZ,...
        wristF, elbowF, shoulderF,...
        -wristNJM, elbowIa, elbowFeR, elbowFwR, elbowNJM,...
        -elbowNJM, shoulderIa, shoulderFsR, shoulderFeR, shoulderNJM],...
        'VariableNames', {'wrist_x', 'wrist_y', 'wrist_z',...
        'elbow_x', 'elbow_y', 'elbow_z',...
        'shoulder_x', 'shoulder_y', 'shoulder_z',...
        'wrist_njf_x', 'wrist_njf_y', 'wrist_njf_z',...
        'elbow_njf_x', 'elbow_njf_y', 'elbow_njf_z',...
        'shoulder_njf_x', 'shoulder_njf_y', 'shoulder_njf_z',...
        'forearm_njmd_x', 'forearm_njmd_y', 'forearm_njmd_z',...
        'forearm_Ia_x', 'forearm_Ia_y', 'forearm_Ia_z',...
        'forearm_md_x', 'forearm_md_y', 'forearm_md_z',...
        'forearm_mp_x', 'forearm_mp_y', 'forearm_mp_z',...
        'forearm_njmp_x', 'forearm_njmp_y', 'forearm_njmp_z',...
        'upperarm_njmd_x', 'upperarm_njmd_y', 'upperarm_njmd_z',...
        'upperarm_Ia_x', 'upperarm_Ia_y', 'upperarm_Ia_z',...
        'upperarm_md_x', 'upperarm_md_y', 'upperarm_md_z',...
        'upperarm_mp_x', 'upperarm_mp_y', 'upperarm_mp_z',...
        'upperarm_njmp_x', 'upperarm_njmp_y', 'upperarm_njmp_z'});
    % create table for second sheet
    tab2_out = array2table([elbow_ang_CROP, [0; elb_vel_CROP'], torso_ang_CROP, [0; torso_vel_CROP']],...
        'VariableNames', {'elbow_angle', 'elbow_angvel', 'torso_angle', 'torso_angvel'});
    % create table for second sheet
    tab3_out = array2table([RF_angleForearmCROP, RF_mag_CROP],...
        'VariableNames', {'rf_angle2forearm', 'rf_mag'});
    
    % create new file name
    filename = ['../subject_data/', files(cnt).name(1:length(files(cnt).name)-10), '.xlsx'];
    writetable(tab1_out, filename, 'sheet', 1)
    writetable(tab2_out, filename, 'sheet', 2)
    writetable(tab3_out, filename, 'sheet', 3)
    
    % clear vars
    clearvars -except files cnt
end