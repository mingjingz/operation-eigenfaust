function opeig
% opeig - The main file for Operation Eigenfaust 3D for MATLAB
% Usage:
% ------
%  opeig;

% History
% -------
% Date              Updater             Modification
% ----              -------             ------------
% Jun 14, 2013      M. Zhang        v0.35: Correct texture projection
% Jun 15, 2013      M. Zhang        v0.40: Correct rendering implemented
% Jun 15, 2013      M. Zhang        v0.45: Attempted Collision Detection
%                                          Added 'Enter' - mouse on/off
%                                           'Escape' - quit
% Jun 20, 2013      M. Zhang        v0.50: Now supports arbitrary resolution
%                                          Added mouse wheel support
% Jun 21, 2013      M. Zhang        v0.55: Added rendering for doors
%                                          (facing only left and right)
%                                          Collision detection completed
% Jun 27, 2013      M. Zhang        v0.60: Improve the variable monitor to 
%                                          allow multiple variables
%                                          Corrected a bug where only the
%                                          most recently pressed dir key is
%                                          effective
%                                          Added 'Tab' - var monitor on/off
% Jul 11, 2013      M. Zhang        v0.65: The door can now be half opened
%                                          (only applies to doors on the 
%                                           right side of the player)
%                                          However, the rendering of scene
%                                          behind an opened door is
%                                          completely wrong: All the
%                                          'horizontal' walls are rendered
%                                          'vertically'.
% Jul 11, 2013      M. Zhang        v0.68: Corrected the rendering of walls
%                                          behind the opened door
% Jul 11, 2013      M. Zhang        v0.75: Now the door to the 'left' of
%                                           the player can be properly
%                                           rendered (with texture flipped)
% Jul 13, 2013      M. Zhang        v0.76: Extend 'processPlayer' to find
%                                          out the block type right in
%                                          front of the player character
% Jul 15, 2013      M. Zhang        v0.80: Now the doors can be opened and
%                                          closed. Although they wont close
%                                          by themselves.
% Jul 18, 2013      M. Zhang        v0.85: Change the name of the demo to
%                                           'Operation Eigenfaust 3D';  
%                                          and all the variable/resource 
%                                          file names accordingly;
%                                          
% ----              -------             ------------
% Copyright (C) Stellari Studio, 2013
% Mingjing Zhang @ Vision & Media Lab, Simon Fraser University, Canada

%% Variable Declaration
try
    Opeig = '0.85';     % Last updated Jul 18, 2013
    MainAxesSize = [];    % The size of the main axes, same as GAME_RESOLUTION
    FPS = [];             % Frames-per-second, ideally over 60
    
    FRAME_DURATION = [];  % The duration of one single frame, ideally less than 1/60
    MAX_FRAME_SKIP = [];  % The maximum of frame skips allowed if the game runs sluggishly
    
    MainFigureInitPos = [];  % The initial position of the main figure
    MainFigureSize = [];  % The size of the figure
    MainAxesInitPos = []; % The initial position of the axes IN the figure
    
    % Handles
    MainFigureHdl = [];
    MainAxesHdl = [];
    SceneHandle = [];
    
    % Keyboard-related variables
    KeyStatus = [];
    LastKeyStatus = [];
    KeyNames = [];
    
    % Variables for Debugging
    ShowFPS = false;
    SHOWFPS_FRAMES = 60;
    
    % Collision Flags
    CloseReq = false;
    
    key = [];
    
    % player character
    
    % Canvas
    EmptyCanvas = [];
    CurCanvas = [];
    
    %% Raycasting & Rendering related variables
    view = [];
    scr_res = [];
    wall_texture = [];   % A 64 x whatever strip that contains all possible wall textures
    cmap = [];          % The global palette
    
    texture_wid = [];
    texture_hgt = [];
    
    xx = [];
    yy = [];
    yy_p = [];
    yy_text = [];
    yy_standard = [];
    apparent_h = [];
    h_scale_factor = [];
    cam_angle_comp = [];
    ones200 = [];       % ones(1,200)
    DEG2RAD = pi/180;
    screenCenter = [];
    use_mouse = true;
    n_obs = [];
    obs_pos = [];
    niche = [];
    niche_pos = [];
    encroach_pos = [];
   fw_vec = [];   % The vector from the current position to the next position
   
   fobj_ind = []; % The index of the object player is facing in the world array
   world_door = []; % A world-sized array where each door block is labelled 1
   all_doors = [];
   myalldoors = [];
   
   all_door_timers = [];
   
    %% Initialization
    initVariable;
    initWindow;
    
    % Variable monitors
    % Help Texts:
    ESC = 'Quit';
    ENTER = 'Mouse On/Off';
    SCROLL_WHEEL = 'Zoom In/Out'
    varname = {'pos','KeyStatus', 'world(fobj_ind)', 'ESC','ENTER','SCROLL_WHEEL'};
    n_varname = numel(varname);
    
    StageList = {'floor1'};
    
    %% Main Game Cycle
    for i_stage = 1:length(StageList)
        %% Load the current stage
        StageTemp = load('OPEIG_WAD.mat', StageList{i_stage});
        CurrentStage = StageTemp.(StageList{i_stage});
        
        %% Setup the map and the walls
        world = CurrentStage.world;
        world_size =CurrentStage.world_size;
        wall = CurrentStage.wall;
        wallD = CurrentStage.wallD;
        wallU = CurrentStage.wallU;
        wallL = CurrentStage.wallL;
        wallR = CurrentStage.wallR;
        world_door = world>=2 & world <=5;
        world_door_status = zeros(size(world)); % 1 - opening, -1 - closing
                                          % 0 - hold
        world_timers = zeros(size(world));
        % An array of timers for all the sliding doors
%         all_door_locs = find(world_door); % 2-3, 4-5 are for sliding doors
        
        % The status of all doors
%         all_door_openness = mod(world(all_door_locs),2); % 2-3->0-1 4-5 ->0-1
        
%         all_door_status = ones(size(all_door_locs)); % 1 - opening, -1 - closing
                                                    % 0 - hold
        %
%         all_door_timers = zeros(size(all_door_locs));
        
        
        readTextures;
        colormap(cmap);
        pos = CurrentStage.init_pos;
        facing = CurrentStage.init_facing;
        
        %% Main Game Loop for each stage
        
        frame_updated = false;   % Whether the world is actually updated
        
        CurrentFrameNo = 0;      % the number of the current frame
        
%         if ShowFPS
        if ShowFPS
            vis = 'on';
        else
            vis = 'off';
        end
        fps_text_handle = text(10,10, '','Visible',vis, 'Interpreter','none');
        var_text_handle = zeros(1, n_varname);  % Pre-allocate text monitor handles
        for itext = 1:n_varname
            var_text_handle(itext) = text(10,10 + 10*itext, '',...
                'Visible',vis,...
                'Interpreter','none'); % Display a variable
        end
        total_frame_update = 0;
%         end
        
        stageStartTime = tic;
        c = stageStartTime;
        FPS_lastTime = toc(stageStartTime);
        terminateFlag = false;
        while 1
            loops = 0;
            curTime = toc(stageStartTime);
            while (curTime >= ((CurrentFrameNo) * FRAME_DURATION) && loops < MAX_FRAME_SKIP)
                %% Process Player
                if ~terminateFlag
                    % Process the movement of the players
                    [pos, facing, fobj_ind] = processPlayer(pos, facing);
                    world = world + world_door_status * 2 / FPS; % change doors
                    world(world >= 3 & world < 4) = 3; % clip 
                    world(world >= 4 & world < 5) = 5;
                    world(world > 1 & world <= 2) = 2;
                    world(world > 3 & world <= 4) = 4;
                    
                    [dist_all, wall_type, all_walls] = stl_RayCasting(pos, facing);
                end
                
                CurrentFrameNo = CurrentFrameNo + 1;
                loops = loops + 1;
                frame_updated = true;
            end
            
            %% Redraw the frame if the world has been processed
            if frame_updated
                renderScene(dist_all, wall_type, all_walls);
                drawnow;
                c = toc(stageStartTime);
                frame_updated = false;
                %
                total_frame_update = total_frame_update + 1;
                
                if mod(total_frame_update,SHOWFPS_FRAMES) == 0 % If time to update fps
                    set(fps_text_handle, 'String',sprintf('FPS: %.2f',SHOWFPS_FRAMES./(c-FPS_lastTime)));
                    FPS_lastTime = toc(stageStartTime);
                end
                if ShowFPS
                    for itext = 1:n_varname
                        set(var_text_handle(itext), 'String', sprintf('%s = %s', varname{itext}, num2str(eval(varname{itext}))));
                    end
                end
            end
            if CloseReq
                delete(MainFigureHdl);
%                 clear all;
                return;
            end
        end
    end
catch err
    delete(MainFigureHdl);
    rethrow(err);
end
%% ---------------- Regular Subfunctions ----------------------------------

%% Initializations

    function initVariable()
        % initVariable - initialize variables
        
        MainAxesSize = [320 200];
        
        maxWinSize = [800 600];
        
        FPS = 60;
        
        FRAME_DURATION = 1./FPS;
        MAX_FRAME_SKIP = 5;
        %         DEFAULT_FRAME_SKIP = 2;
        MainFigureSize = MainAxesSize .* 2;
        maxReal2max = max(MainFigureSize./maxWinSize);  % 
        if maxReal2max > 1  % at least one of them exceeds the maximum window
            MainFigureSize = MainFigureSize ./ maxReal2max;
        end
        MainFigureInitPos = [300 50];
        MainAxesInitPos = [0 0];
        
        KeyNames = {'w','s','a','d','leftarrow','rightarrow','space','return',...
            'escape','tab'};
        %         KeyFuncs = {'up','down','left','right','sprint','jump','start','select'};
        KeyStatus = false(1, length(KeyNames));
        LastKeyStatus = KeyStatus;
        ShowFPS = true;

        view.angle = 90;
        view.dist = 0.25;
        h_scale_factor =  tan(DEG2RAD * view.angle /2);
        view.plane_size = 2 * view.dist *h_scale_factor;
        
        % opeigview = viewdef; % Get the 3D viewing setting
        view.resx = MainAxesSize(1); % 320 pixels
        view.resy = MainAxesSize(2);
        
        scr_res = -view.resx/2:view.resx/2-1;  % number pixels from -160-159
        
        texture_wid = 64;
        texture_hgt = 64;
        % The angle of each ray relative to the facing vector
        cam_angle_comp = atan((scr_res+0.5) / view.resx * view.plane_size./view.dist);
        
        [xx, yy] = meshgrid(1:view.resx,1:view.resy);
        yy_p = (yy(:,:) - view.resy/2)./(view.resy/2);
        [~, yy_text] = meshgrid(1:view.resx, linspace(1,texture_hgt,view.resy));
        [~, yy_standard] = meshgrid(1:view.resx, linspace(0,1,view.resy));
        yy_text = round(yy_text);
        EmptyCanvas = uint8([ones(view.resy./2, view.resx);zeros(view.resy./2, view.resx)]);
        CurCanvas = uint8(zeros(view.resy, view.resx));
        
        ones200 = ones(1,view.resy);
    end

    function readTextures()
    % READTEXTURES: Load the wall textures
        wall_texture = load('OPEIG_WALLS.mat');
        
        % Discard the wall textures which definitely won't be used.
        wall_texture = wall_texture.wall_texture(:,1:max(wall(:))*texture_wid,:);
        floor_color = [0.7 0.7 0.7];
        ceiling_color = [0.5 0.5 0.5];
        
        % Convert the wall texture to index images. Use only 254 colors 
        % The other 2 colors are reserved for floor and ceiling.
        [wall_texture, cmap] = rgb2ind(wall_texture,254);
        wall_texture = wall_texture + 2;
        cmap = [floor_color; ceiling_color; cmap];
    end

    function initWindow()
        % initWindow - initialize the main window, axes and image objects
        MainFigureHdl = figure('Name', ['Operation Eigenfaust 3D MAT ' Opeig], ...
            'NumberTitle' ,'off', ...
            'Units', 'pixels', ...
            'Position', [MainFigureInitPos, MainFigureSize], ...
            'MenuBar', 'figure', ...
            'Renderer', 'Painter',...\
            'UserData', 'opeig',...
            'KeyPressFcn', @stl_KeyDown,...
            'KeyReleaseFcn', @stl_KeyUp, ...
            'WindowScrollWheelFcn', @stl_MouseWheel); %,...
        %             'CloseRequestFcn', @stl_CloseReqFcn);
        MainAxesHdl = axes('Parent', MainFigureHdl, ...
            'Units', 'normalized',...
            'Position', [MainAxesInitPos, 1-MainAxesInitPos.*2], ...
            'color', [0 0 0], ...
            'XLim', [0 MainAxesSize(1)]-0.5, ...
            'YLim', [0 MainAxesSize(2)]-0.5, ...
            'YDir', 'reverse', ...
            'NextPlot', 'add', ...
            'Visible', 'on', ...
            'XTick',[], ...
            'YTick',[]);
        SceneHandle = image(0, 0, [],...
            'Parent', MainAxesHdl,...
            'Visible', 'on');

        screenCenter = get(0,'ScreenSize');
        screenCenter = screenCenter(3:4)./2;
        set(0,'PointerLocation',screenCenter)
    end

%% Game logics
    function [pos, facing, facing_obj_ind] = processPlayer(pos, facing)
        %
        unit_facing_vec = [cos(facing * DEG2RAD), sin(facing * DEG2RAD)];
        fw_vec = unit_facing_vec * 2/FPS;
        left_vec = fw_vec * [0 -1;1 0]; % rotate -90 deg
        rot_angle = 60/FPS;
        new_pos = pos;
        if KeyStatus(1)
            new_pos = new_pos + fw_vec;
        end
        if KeyStatus(2)
            new_pos = new_pos - fw_vec;
        end
        if KeyStatus(3)
            new_pos = new_pos + left_vec;
        end
        if KeyStatus(4)
            new_pos = new_pos - left_vec;
        end
        if KeyStatus(5)
            facing = facing - rot_angle;
        end
        if KeyStatus(6)
            facing = facing + rot_angle;
        end
        
        if use_mouse
            curMousePos = get(0, 'PointerLocation');
            curMouseDispl = curMousePos - screenCenter;
            set(0,'PointerLocation',screenCenter)
            facing = facing + curMouseDispl(1);
        end
        facing = mod(facing, 360);
        
        %% Collision Detection
%         if false
        temp_new_pos = new_pos+0.5; %[floor(new_pos(1) + 0.5), floor(new_pos(2)+0.5)];
        niche_pos =  [floor(temp_new_pos); ceil(temp_new_pos)];
        niche = world(niche_pos(:,2), niche_pos(:,1));
        [obs_pos_r, obs_pos_c] = find(niche~=0&niche~=3&niche~=5);
        obs_pos = (obs_pos_c-1)*2+obs_pos_r;
        n_obs = numel(obs_pos);
        if n_obs == 3       % In a corner (3 blocks)
            new_pos = round(new_pos + 0.5) - 0.5;
        elseif n_obs == 2   % Stopped by a wall (2 blocks)
            thiswalltype = obs_pos(2) - obs_pos(1);
            if  thiswalltype == 1   % a Vertical wall
                new_pos(1) = round(new_pos(1) + 0.5) - 0.5;
            elseif thiswalltype == 2  % a horizontal wall
                new_pos(2) = round(new_pos(2) + 0.5) - 0.5;
            elseif thiswalltype == 3  % diagonal ... ?
                new_pos = round(new_pos + 0.5) - 0.5;
            end
        elseif n_obs == 1   % Only 1 block
            % The player will be pushed back to a most reasonable location
            x = obs_pos_c;
            y = obs_pos_r;
            block_pos = [ niche_pos(x,1), niche_pos(y,2)];

            [~, pushed_pos] = circle_square_intersect(block_pos, temp_new_pos);
            new_pos = pushed_pos-0.5;
        elseif n_obs == 4
%             error(' you end up in the wall..');
        end
        pos = new_pos;
        
        % The object the player is facing.
        obj_pos = pos + unit_facing_vec./sqrt(2); % must be within distance sqrt(2) 
        if all(floor(obj_pos) == floor(pos)) 
            % If the end of the 'facing' vector is in the same block as
            % player
            facing_obj_ind = [];
        else
            obj_pos = ceil(obj_pos);
            facing_obj_ind = (obj_pos(1)-1)*world_size(1) + obj_pos(2);
        end
    end
%% Perform raycasting for a frame

    function [dist_all, int_wall_type, all_walls] = stl_RayCasting(pos, facing)
        
        cur_view.angle = DEG2RAD * (facing) + cam_angle_comp;
        
        % Precalculate all the cos/sin/tans
        cur_view.angle_cos = cos(cur_view.angle);
        cur_view.angle_sin = sin(cur_view.angle);
        cur_view.angle_tan = tan(cur_view.angle);
        
        %% Find intersections on horizontal walls
        faceup = cur_view.angle_sin < 0;        % Those rays that point upward
        facedown = cur_view.angle_sin > 0;      % Those that go downward
        
        nup = sum(faceup);
        ndown = sum(facedown);
        
        %{ player pos
        %  -0---1-2----------------
        %  |  |  |  |
        %  0  1  2  3 ...
        %  |  |  |  |
        %  -1---1-2----------------
        %}
        
        %% UP and DOWN
        % Y coord of the first intersection in the UP and DOWN directions
        yup = ceil(pos(2)-1);    % Ensures correct calculation if
        ydown = floor(pos(2)+1); % player happens to be on a boundary
        
        %The Y coord of the first intersections for ALL rays
        yups = yup(1,ones(1,nup));
        % Calculate the corresponding x coords
        xups = (pos(1) + (yups - pos(2))./cur_view.angle_tan(faceup));
        
        % Do the same for downward rays
        ydowns = ydown(1, ones(1, ndown));
        xdowns = (pos(1) + (ydown - pos(2))./cur_view.angle_tan(facedown));
        
        % The final (x,y) for intersections:
        yhoris = zeros(1, view.resx)+Inf;
        xhoris = zeros(1, view.resx)+Inf;
        wall_horis = zeros(1, view.resx);
        
        % Init the intersection flags
        cur_intersect = false(1,nup);
        cur_non_intersect = ~cur_intersect;
        no_intersect = false(1, nup);
        wall_up = zeros(1,nup);   % The texture id of each intersection
        
        cur_ints_d = false(1, ndown);
        cur_non_ints_d = ~cur_ints_d;
        no_ints_d = ~false(1, ndown);
        wall_down = zeros(1,ndown);
        
        cur_inv_tan = 1./cur_view.angle_tan(faceup);
        cur_inv_tan_d = 1./cur_view.angle_tan(facedown);
        
        %% Process UPWARD first
        
        % Flags that are set to true when the rays intersect with a wall.
        for ii = 1:1
            
            cur_intersect(:) = false;
            cur_non_intersect(:) = ~cur_intersect(:);
            no_intersect(:) = false;
            k = floor(xups(cur_non_intersect))+1;
            no_intersect(cur_non_intersect) = (yup < 1) | ...          % if y is too small
                (floor(xups(cur_non_intersect))+1 > world_size(2)) | ...   % if x is too large
                (floor(xups(cur_non_intersect))+1 < 1);
                        
            while 1
                % If it is impossible for a ray to make an intersection with 
                % a horizontal wall within the range of the map
                % then its 'no_intersect' flag will be set to true, 
                % treating them as 'already intersected', and thus will not
                % be processed in the next iteration.
                
                no_intersect(cur_non_intersect) = (yup < 1) | ...          % if y is too small
                    (floor(xups(cur_non_intersect))+1 > world_size(2)) | ...   % if x is too large
                    (floor(xups(cur_non_intersect))+1 < 1);                % if x is too small
                
                cur_intersect = cur_intersect | no_intersect;
                cur_non_intersect = ~cur_intersect;
                
                if all(cur_intersect)
                    break;
                end
                
                % Check if the intersection is a wall
                % for all the non-intersected rays
                cur_intersect(cur_non_intersect) = ...
                    world(yup, floor(xups(cur_non_intersect))+1);
                
                % Find those that still aren't intersected
                cur_non_intersect = ~cur_intersect;
                
                %  scan the next possible point
                yup = yup - 1;
                yups(cur_non_intersect) = yup;
                
                % using sign always generates 1 or -1, since xups/xdowns do not include
                % 0 cases.
                xups(cur_non_intersect) = xups(cur_non_intersect) - ...
                    cur_inv_tan(cur_non_intersect);
            end
            
            xups(no_intersect) = Inf;
            yups(no_intersect) = Inf;
            % Get the texture
            xups_int = floor(xups(~no_intersect));
            ind = world_size(1) * xups_int +yups(~no_intersect); % No + 1
            wall_up(~no_intersect) = (wallD(ind)-1) * texture_wid + ... % The texture index
                floor((xups(~no_intersect) - xups_int) * texture_wid)+1; % The column index
            
            % The wall texture column the rays encounters on their way up
            
            % wallups(~no_intersect) = wall(sub2ind(world_size, yups(~no_intersect),...
            %     ceil(xups(~no_intersect))));
            % xups
            %toc;
            % disp('Up over');
            % visual_raycast(world, pos, xups, yups)
            %% Then process DOWNWARD
            % Flags that are set to true when the rays intersect with a wall.
            cur_ints_d(:) = false;
            cur_non_ints_d(:) = ~cur_ints_d(:);
            no_ints_d(:) = false;
            while 1
                % If it is impossible for a ray to make an intersection with 
                % a horizontal wall within the range of the map
                % then its 'no_ints_d' flag will be set to true, 
                % treating them as 'already intersected', and thus will not
                % be processed in the next iteration.
                no_ints_d(cur_non_ints_d) = (ydown > world_size(1)-1) | ...          % if y is too small
                    (floor(xdowns(cur_non_ints_d))+1 > world_size(2)) | ...   % if x is too large
                    (floor(xdowns(cur_non_ints_d))+1 < 1);                % if x is too small
                
                cur_ints_d = cur_ints_d | no_ints_d;
                cur_non_ints_d = ~cur_ints_d;
                if all(cur_ints_d)
                    break;
                end
                
                % Check if the intersection is a wall
                % for all the non-intersected rays
                % For downward rays, 'ydown+1' means takes the
                % block 'below' the intersected wall.
                cur_ints_d(cur_non_ints_d) = ...
                    world(ydown+1, floor(xdowns(cur_non_ints_d))+1);
                
                % Find those that still aren't intersected
                cur_non_ints_d = ~cur_ints_d;
                
                %  scan the next possible point
                ydown = ydown + 1;
                ydowns(cur_non_ints_d) = ydown;
                xdowns(cur_non_ints_d) = xdowns(cur_non_ints_d) + ...
                    cur_inv_tan_d(cur_non_ints_d);
            end
            xdowns(no_ints_d) = Inf;
            ydowns(no_ints_d) = Inf;
            
            % Get the texture
            % ind = world_size(1)*xdowns_int+ydowns(~no_ints_d)+1;
            % wall_down(~no_ints_d) = wallU(ind);
            % Get the texture
            xdowns_int = floor(xdowns(~no_ints_d));
            ind = world_size(1)*xdowns_int+ydowns(~no_ints_d)+1; % Have to +1 because the block is below the intersection
            wall_down(~no_ints_d) = ((wallU(ind)-1)*texture_wid) + floor((1- xdowns(~no_ints_d) + xdowns_int) * texture_wid)+1; % The wall texture index the rays encounters on their way up
            % if the ray goes down, it means the texture starts from the right. Hence '1- xdowns(~no_ints_d) + xdowns_int'
            
            %toc;
            % visual_raycast(world, pos, xdowns, ydowns)
            
            %% Combine UP and DOWN together
            
            %% ---------------------------------------
            %% ---------------------------------------
            %% ---------- Now find intersections on Verticle Walls
            %% -----------------------------------------
            %% ------------------------------------------
            %% Find intersections on horizontal walls
            faceleft = cur_view.angle_cos < 0;        % Those rays that point leftward
            faceright = cur_view.angle_cos > 0;      % Those that go rightward
            
            nleft = sum(faceleft);
            nright = sum(faceright);
            
            %% LEFT and RIGHT
            % x coord of the first intersection in the LEFT-RIGHT directions
            xleft = ceil(pos(1)-1);    % Ensures correct calculation if
            xright = floor(pos(1)+1); % player happens to be on a boundary
            
            %The Y coord of the first intersections for ALL leftward rays
            xlefts = xleft(1,ones(1,nleft));
            % Calculate the corresponding x coords
            ylefts = (pos(2) + (xlefts - pos(1)) .* cur_view.angle_tan(faceleft));
            
            % Do the same for rightward rays
            xrights = xright(1, ones(1, nright));
            yrights = (pos(2) + (xrights - pos(1)).*cur_view.angle_tan(faceright));
            
            % The final (x,y) for ALL intersections:
            yverts = zeros(1, view.resx) + Inf;
            xverts = zeros(1, view.resx) + Inf;
            wall_verts = zeros(1, view.resx);
            
            cur_ints_l = false(1,nleft);
            cur_non_ints_l = ~cur_ints_l;
            no_ints_l = false(1, nleft);
            cur_tan_l = cur_view.angle_tan(faceleft);
            wall_left = zeros(1, nleft);
            
            cur_ints_r = zeros(1, nright);
            cur_non_ints_r = ~cur_ints_r;
            no_ints_r = ~false(1, nright);
            cur_tan_r = cur_view.angle_tan(faceright);
            wall_right = zeros(1, nright);
            
            %% Process LEFTward first
            
            % Flags that are set to true when the rays intersect with a wall.
            cur_ints_l(:) = false;
            cur_non_ints_l(:) = ~cur_ints_l(:);
            no_ints_l(:) = false;
            %tic;
            while 1
                % If a ray is impossible to make an intersection with a vertical wall
                % then its 'no_ints_l' flag will be set to true, treating them as
                % 'already intersected'
                no_ints_l(cur_non_ints_l) = (xleft < 1) | ...          % if x is too small
                    (floor(ylefts(cur_non_ints_l))+1 > world_size(1)) | ...   % if y is too large
                    (floor(ylefts(cur_non_ints_l))+1 < 1);                % if y is too small
                
                cur_ints_l = double(cur_ints_l | no_ints_l);
                cur_non_ints_l = ~cur_ints_l;
                if all(cur_ints_l)
                    break;
                end
                
                % Check if the intersection is a wall
                % for all the non-intersected rays
                cur_ints_l(cur_non_ints_l) = ...
                    world(floor(ylefts(cur_non_ints_l))+1, xleft);

                % a value between 2-3 indicates a vertical door.                
                vert_doors = (cur_ints_l>=2 & cur_ints_l<=3); 
                
                % how much a door is opened
                vert_doors_open = zeros(size(cur_ints_l)); 
                vert_doors_open(vert_doors) = cur_ints_l(vert_doors) - 2;
%                 vert_doors_open = cur_ints_l - floor(cur_ints_l);
                
                if any(vert_doors)  % Indent 0.5 for doors
                    xlefts(vert_doors) = xleft - 0.5;
                    ylefts(vert_doors) = ylefts(vert_doors) - 0.5*cur_tan_l(vert_doors);
                    
                    % The decimal part of all y values,
                    % used to determine if a ray would pass a half opened 
                    % door or not.
                    y_decimal = mod(ylefts,1);

                    % If a ray happens to fall on the opened part of door
                    rays_possible_crack = (y_decimal > 0 & y_decimal < vert_doors_open);
                    rays_crack = vert_doors & rays_possible_crack;
                    
                    xlefts(rays_crack) = xleft;
                    ylefts(rays_crack) =  ylefts(rays_crack) + 0.5*cur_tan_l(rays_crack);
                    cur_ints_l(rays_crack) = 0; % Continue to propagate those rays
                end
                % Find those that still aren't intersected
                cur_non_ints_l = ~cur_ints_l;
                
                %  scan the next possible point (the one on the left)
                xleft = xleft - 1;
                xlefts(cur_non_ints_l) = xleft;
                
                % using sign always generates 1 or -1, since there are no
                % 0 cases.
                ylefts(cur_non_ints_l) = ylefts(cur_non_ints_l) -...
                    cur_tan_l(cur_non_ints_l);
                
                %     no_intersect = ylefts(floor(ylefts)+1 > world_size(2)) = 0;
                %     yup(yup < 1) = 1;
            end
            no_ints_l = no_ints_l | ((ylefts < 0) | (ylefts >  world_size(1)));
            
            
            ylefts(no_ints_l) = Inf;
            xlefts(no_ints_l) = Inf;
            %toc;
            % Get the texture
            ylefts_int = floor(ylefts(~no_ints_l));
            ind = world_size(1) * (ceil(xlefts(~no_ints_l)-1)) + ylefts_int+1; % -1 for the block on the 'left' of the wall
            ind(ind==0) = 1;
            wall_left(~no_ints_l) = (wallR(ind)-1) * texture_wid + ... % The texture index
                floor((1 - ylefts(~no_ints_l) + ylefts_int) * texture_wid)+1; % The column index
            
            worldshift = mod(world,1);
            
            % Those rays the intersects with an half-opened door
            all_doors = world>=2 & world<=3;

            vis_world = worldshift(ind); % Visual part of the world; 1 x n
            doors_in_view = all_doors(ind);
            
%             all_doors = vis_world~=0; % 1 x n logical
%             myalldoors = ind(all_doors);
%              all_shifts = vis_world(all_doors); % 1 x k
            % doors in the world
%             wall_right(~no_ints_r) = (wallL(ind)-1) * texture_wid + ... % The texture index
%                 mod(floor((yrights(~no_ints_r) - yrights_int - worldshift(ind)) * texture_wid), texture_wid)+1; % The column index
            vis_wall = wallR(ind);
%             texture_ind = (wallR(ind)-1) * texture_wid;
            offsets = ylefts(~no_ints_l) - ylefts_int;
            offsign = ones(size(offsets));
            offsign(doors_in_view) = -1; %(vis_world > 0) = -1;
            onesign = ~doors_in_view; %(vis_world == 0);
            wall_left(~no_ints_l) = (vis_wall-1) * texture_wid + ... % The texture index
                mod(floor((onesign - offsign .* offsets - vis_world) * texture_wid), texture_wid)+1; % The column index
%             wall_left(all_doors) = 1; %(vis_wall(all_doors)-1)*texture_wid + ...
                %mod(floor((ylefts(all_doors) - ylefts_int(all_doors) - all_shifts) * texture_wid), texture_wid) + 1;
            
            if any(all_doors)
                a = 3;
            end
%             wall_left(~no_ints_l) = (wallR(ind)-1) * texture_wid + ... % The texture index
%                 floor((1 - ylefts(~no_ints_l) + ylefts_int) * texture_wid)+1; % The column index
            % If it is a door, then we must reverse its texture
            
%             assert(all(wall_left>=0))
            
            % visual_raycast(world, pos, xlefts, ylefts);
            
            %% Then process RIGHTWARD
            
            % Flags that are set to true when the rays intersect with a wall.
            cur_ints_r(:) = false;
            cur_non_ints_r(:) = ~cur_ints_r(:);
            no_ints_r(:) = false;
            %tic;
            while 1
                % If a ray is impossible to make an intersection with a horizontal wall
                % then its 'no_intersect' flag will be set to true, treating them as
                % 'already intersected'
                no_ints_r(cur_non_ints_r) = (xright + 1 > world_size(2)) | ...          % if x is too large
                    (floor(yrights(cur_non_ints_r))+1 > world_size(1)) | ...   % if y is too large
                    (floor(yrights(cur_non_ints_r))+1 < 1);                % if y is too small
                
                cur_ints_r = double(cur_ints_r | no_ints_r);
                cur_non_ints_r = ~cur_ints_r;
                if all(cur_ints_r)
                    break;
                end
                
                % Check if the intersection is a wall
                % for all the non-intersected rays
                cur_ints_r(cur_non_ints_r) = ...
                    world(floor(yrights(cur_non_ints_r))+1, xright+1);
                
                % If there is any VERTICAL door
                
                vert_doors = (cur_ints_r>=2 & cur_ints_r <=3);

                % how much a door is opened
                vert_doors_open = zeros(size(cur_ints_r)); 
                vert_doors_open(vert_doors) = cur_ints_r(vert_doors) - 2;
%                 cur_ints_r(vert_doors);
%                 cur_ints_r - floor(cur_ints_r);
                
                if any(vert_doors)  % Indent 0.5
                    % y_vert_doors = floor(yrights(vert_doors)); 
                    % all the y values of intersection on doors
                    
                    xrights(vert_doors) = xright + 0.5;
                    yrights(vert_doors) = yrights(vert_doors) + 0.5*cur_tan_r(vert_doors);
                    
                    % The decimal part of all y values,
                    % used to determine if a ray would pass a half opened 
                    % door or not.
                    y_decimal = mod(yrights,1);

                    % If a ray happens to fall on the opened part of door
                    rays_possible_crack = (y_decimal > 0 & y_decimal < vert_doors_open);
                    rays_crack = vert_doors & rays_possible_crack;
                    
                    xrights(rays_crack) = xright;
                    yrights(rays_crack) =  yrights(rays_crack) - 0.5*cur_tan_r(rays_crack);
                    cur_ints_r(rays_crack) = 0; % Continue to propagate those rays
                end
                % If all rays are 'intersected', then quit.
                %     if all(cur_ints_r)
                %         break;
                %     end
                
                % Find those that still aren't intersected
                cur_non_ints_r = ~cur_ints_r;
                
                %  scan the next possible point (the one on the left)
                xright = xright + 1;
                xrights(cur_non_ints_r) = xright;
                
                % using sign always generates 1 or -1, since there are no
                % 0 cases.
                yrights(cur_non_ints_r) = yrights(cur_non_ints_r) +...
                    cur_tan_r(cur_non_ints_r); % + ...
%                 if any(yrights < -100 )
%                     a = 33
%                 end
                %     no_intersect = ylefts(floor(ylefts)+1 > world_size(2)) = 0;
                %     yup(yup < 1) = 1;
            end
            no_ints_r = no_ints_r | ((yrights < 0) | (yrights >  world_size(1)));
            yrights(no_ints_r) = Inf;
            xrights(no_ints_r) = Inf;
            %toc;
            yrights_int = floor(yrights(~no_ints_r));
            
            % The 1D index on the world map.
            ind = world_size(1) * (floor(xrights(~no_ints_r))) + yrights_int + 1 ;
            % has +1 and has no -1 for the block on the 'right' of the wall
            
%             all_vert_door = (world > 2  & world < 3) & ~no_ints_r; % all the vertical
            worldshift = mod(world,1);
            
            % doors in the world
            wall_right(~no_ints_r) = (wallL(ind)-1) * texture_wid + ... % The texture index
                mod(floor((yrights(~no_ints_r) - yrights_int - worldshift(ind)) * texture_wid), texture_wid)+1; % The column index
            
            % visual_raycast(world, pos, xrights, yrights);
            
            %% Combine UP and DOWN together, LEFT and RIGHT together
            xhoris(faceup) = xups;
            yhoris(faceup) = yups;
            
            xhoris(facedown) = xdowns;
            yhoris(facedown) = ydowns;
            
            % wall_up(~no_intersect) = wallD(ind); % The wall texture index the rays encounters on their way up
            % wall_down(~no_ints_d)
            wall_horis(faceup) = wall_up;
            wall_horis(facedown) = wall_down;
            
            xverts(faceleft) = xlefts;
            yverts(faceleft) = ylefts;
            
            xverts(faceright) = xrights;
            yverts(faceright) = yrights;
            
            wall_verts(faceleft) = wall_left;
            wall_verts(faceright) = wall_right;
            
            dist_hori = sqrt((xhoris-pos(1)).^2 + (yhoris-pos(2)).^2);
            dist_vert = sqrt((xverts-pos(1)).^2 + (yverts-pos(2)).^2);
            [dist_all, int_wall_type] = min([dist_hori; dist_vert]);
            all_walls = [wall_horis, wall_verts];
        end
    end

    function renderScene(dist_all, int_wall_type, all_walls)
        % Render the final image !!!!1
        % The apparent height of each wall column on screen (in pixels);
%         apparent_h = round(view.resx.*view.plane_size./(dist_all)./cos(cam_angle_comp));
        h_scale_factor = tan(DEG2RAD * view.angle ./2);
        apparent_h = round(view.resx./2./h_scale_factor./(dist_all)./cos(cam_angle_comp));

        % The y coords where the wall columns start
        start_y_wallcols = ceil((view.resy - apparent_h)./2);
        apparent_h_ary = apparent_h(ones200,:);
        
        % All the wall columns to be mapped on the actual walls
        texture_strip = wall_texture(:,all_walls([1:view.resx] +(int_wall_type-1)*view.resx));
        
        
        full_walls = start_y_wallcols >=0;
        all_yy = round(yy(:,full_walls) + (apparent_h_ary(:,full_walls)-view.resy)./2 .* yy_p(:,full_walls));
        all_yy(all_yy>view.resy) = view.resy;
        all_yy(all_yy<1) = 1;
        
        all_xx = xx(:,full_walls);
        all_yy_text = round(yy_text(:, full_walls));
        all_text_xx = xx(:,~full_walls);
        
        cropped_yy_standard = yy_standard(:, ~full_walls);
        text_strip_stary_y =  repmat(round((-start_y_wallcols)*texture_hgt./apparent_h),[view.resy 1]); % Where the texture y starts
        cropped_yy_text = round(cropped_yy_standard .* ((texture_hgt-1) - 2*text_strip_stary_y(:,~full_walls)) + text_strip_stary_y(:,~full_walls)+1);
        cropped_yy_text(cropped_yy_text<1) = 1;
        cropped_yy_text(cropped_yy_text > texture_hgt) = texture_hgt;
        
        CurCanvas = EmptyCanvas;
        CurCanvas((all_xx(:)-1)*view.resy + all_yy(:)) = texture_strip((all_xx(:)-1)*texture_hgt + all_yy_text(:));
        CurCanvas(:, ~full_walls) = texture_strip((all_text_xx-1)*texture_hgt + cropped_yy_text);
        
        set(SceneHandle, 'CData', CurCanvas);
    end

%% Callback functions
    function stl_KeyUp(hObject, eventdata, handles)
        LastKeyStatus = KeyStatus;
        
        key = get(hObject,'CurrentKey');
        KeyStatus = (~strcmp(key, KeyNames) & LastKeyStatus);
        
    end
    function stl_KeyDown(hObject, eventdata, handles)
        LastKeyStatus = KeyStatus;
        key = get(hObject,'CurrentKey');
        
        KeyStatus = (strcmp(key, KeyNames) | LastKeyStatus);
        
        % If 'space' key is pressed
        if KeyStatus(7)
              if world_door(fobj_ind) % If facing a door
                if world_door_status(fobj_ind) ~= 0 % If the door is sliding
                    % Reverse the process
                    world_door_status(fobj_ind) = - world_door_status(fobj_ind);                    
                else  % If the door is not sliding 
                    if mod(world(fobj_ind),2) == 0 % If the door is fully closed
                        % then start opening
                        world_door_status(fobj_ind) = 1;
                    elseif mod(world(fobj_ind),2) == 1 % If it is fully open
                        % then start closing
                        world_door_status(fobj_ind) = -1;
                    else % If it is halfway open
                        % then do not move
                        world_door_status(fobj_ind) = 1;
                    end
                end
            end
        end
        
        if KeyStatus(8)
            use_mouse = ~use_mouse;
        end
        if KeyStatus(9)
            CloseReq = true;
        end
        % Turn the test info on/off
        if KeyStatus(10) 
            ShowFPS = ~ShowFPS;
            if ShowFPS
                set(var_text_handle,'Visible','on');
                set(fps_text_handle,'Visible','on');
            else
                set(var_text_handle,'Visible','off');
                set(fps_text_handle,'Visible','off');
            end
        end
    end
    function stl_MouseWheel(hObject, eventdata, handles)
        view.angle =  view.angle + eventdata.VerticalScrollCount*1.5;
        view.angle = min(max(view.angle, 10),90);
       
        % Refresh some variables
        h_scale_factor =  tan(DEG2RAD * view.angle /2);
        view.plane_size = 2 * view.dist *h_scale_factor;
        cam_angle_comp = atan((scr_res+0.5) / view.resx * view.plane_size./view.dist);
    end

    function stl_CloseReqFcn(hObject, eventdata, handles)
        CloseReq = true;
    end
%% Utility functions:
    function [interflag, circle_new_pos] = circle_square_intersect(sq_pos, circle_pos)
    % Determines if a circle (player) intersects with a square (wall)
        vec = sq_pos - circle_pos;
        
        abs_vec = abs(vec);
        sign_vec = sign(vec);
%         dist =vec(:)'*vec(:); % Doesn't seem to be used?
        interflag = false;
        circle_new_pos = circle_pos;
        
        if all(abs_vec >= 1)    % No intersection
            return;
        end
        
        if abs_vec(1)<=0.5
            if abs_vec(2) < 1
                interflag = true;
                circle_new_pos(2) = sq_pos(2) - [sign_vec(2)];
                return
            else
                return
            end
        elseif abs_vec(2) <= 0.5
            if abs_vec(1) < 1
                interflag = true;
                circle_new_pos(1) = sq_pos(1) - [sign_vec(1)];                
                return
            else
                return
            end
        else
            corner_pos = sq_pos - sign_vec.*0.5; 
            vec2 = corner_pos - circle_pos;
%             sign_vec2 = sign(vec2);  % doesn't seem to be used
            dist2 = sqrt(vec2(:)'*vec2(:));
            if dist2 < 0.5
                interflag = true;
                circle_new_pos = corner_pos - vec2./dist2*0.5;
                return
            end
        end
            
    end

end


