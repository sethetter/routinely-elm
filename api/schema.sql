begin;

create table actions (
       id            serial,
       name          varchar(64) not null,
       value         integer not null default 1,
       per_week      integer not null default 5,
       per_day       integer not null default 1
);

create table action_logs (
       id            serial,
       name          varchar(64) not null,
       value         integer not null default 1,
       action_id     integer not null,
       created_at    timestamp not null default current_timestamp
);

create or replace function create_action_log (action_id integer)
returns void as $$
begin
       insert into action_logs (action_id, name, value)
       values(
        action_id,
        (select name from actions where id = action_id limit 1),
        (select value from actions where id = action_id limit 1)
       );
       select
end
$$ language 'plpgsql';

commit;
