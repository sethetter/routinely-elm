begin;

create table actions (
       id            serial,
       name          varchar(64) not null,
       value         integer not null default 1,
       week_days     varchar(7) not null default '1111100'
);

create table action_logs (
       id            serial,
       name          varchar(64) not null,
       value         integer not null default 1,
       action_id     integer,
       confirmed_at  timestamp,
       created_at    timestamp not null default current_timestamp,
       updated_at    timestamp not null default current_timestamp
);

commit;
