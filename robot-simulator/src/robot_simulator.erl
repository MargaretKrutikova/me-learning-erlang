-module(robot_simulator).

-export([advance/1,
         create/0,
         direction/1,
         left/1,
         place/3,
         position/1,
         right/1]).

-type direction() :: north | south | east | west.

-record(robot,
        {x = 0 :: number(),
         y = 0 :: number(),
         direction = north :: direction()}).

% ------- api -----

create() -> spawn(fun start_robot_process/0).

advance(RobotPid) -> call_process(RobotPid, advance).

direction(RobotPid) ->
    call_process(RobotPid, direction).

left(RobotPid) -> call_process(RobotPid, turn_left).

right(RobotPid) -> call_process(RobotPid, turn_right).

place(RobotPid, Direction, Position) ->
    call_process(RobotPid, {place, Direction, Position}).

position(RobotPid) -> call_process(RobotPid, position).

%---- robot helper function -----

direction_turn(Direction, Turn) when Turn == left ->
    case Direction of
        north -> west;
        south -> east;
        east -> north;
        west -> south
    end;
direction_turn(Direction, Turn) when Turn == right ->
    case Direction of
        north -> east;
        south -> west;
        east -> south;
        west -> north
    end.

robot_advance(Robot = #robot{x = X, y = Y,
                             direction = Direction}) ->
    case Direction of
        north -> Robot#robot{y = Y + 1};
        south -> Robot#robot{y = Y - 1};
        east -> Robot#robot{x = X + 1};
        west -> Robot#robot{x = X - 1}
    end.

robot_turn(Robot, Turn) ->
    Robot#robot{direction =
                    direction_turn(Robot#robot.direction, Turn)}.

create_robot() -> #robot{}.

create_robot(Direction, {X, Y}) ->
    #robot{direction = Direction, x = X, y = Y}.

robot_position(#robot{x = X, y = Y}) -> {X, Y}.

robot_direction(#robot{direction = Direction}) ->
    Direction.

% ------ process ------

handle_message(Robot, Message) ->
    case Message of
        advance -> {robot_advance(Robot), ok};
        turn_left -> {robot_turn(Robot, left), ok};
        turn_right -> {robot_turn(Robot, right), ok};
        {place, Direction, Position} ->
            {create_robot(Direction, Position), ok};
        position -> {Robot, robot_position(Robot)};
        direction -> {Robot, robot_direction(Robot)}
    end.

robot_process(Robot) ->
    receive
        {From, Message} ->
            {NextState, Result} = handle_message(Robot, Message),
            From ! Result,
            robot_process(NextState)
    end.

call_process(Pid, Message) ->
    Pid ! {self(), Message},
    receive Msg -> Msg after 500 -> {error, timeout} end.

start_robot_process() -> robot_process(create_robot()).
