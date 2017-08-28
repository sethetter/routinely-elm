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
       action_id     integer,
       created_at    timestamp not null default current_timestamp
);

commit;
