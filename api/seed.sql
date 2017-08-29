insert into actions (name, value, per_week, per_day)
  values ('Brush teeth', 1, 5, 2);

insert into actions (name, value, per_week, per_day)
  values ('Clean room', 3, 1, 1);

insert into actions (name, value, per_week, per_day)
  values ('Homework', 4, 1, 1);

-- Seed fake logs

insert into action_logs (name, value, action_id, created_at)
  values ('Brush teeth', 1, 1, current_timestamp);

insert into action_logs (name, value, action_id, created_at)
  values ('Brush teeth', 1, 1, current_timestamp);

insert into action_logs (name, value, action_id, created_at)
  values ('Clean room', 3, 2, date '08/29/17' + interval '1 hour');
