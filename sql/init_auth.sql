-- /sql/init_auth.sql — inicializace metadat (PostgreSQL)
-- POZN.: "create extension citext" může vyžadovat superusera / vlastníka DB.

create schema if not exists app_meta;
create extension if not exists citext;

create table if not exists app_meta.user_account(
  user_id        bigserial primary key,
  email          citext not null unique,
  display_name   text not null,
  password_hash  text not null,
  is_active      boolean not null default true,
  created_at     timestamptz not null default now(),
  updated_at     timestamptz not null default now(),
  last_login_at  timestamptz,
  mfa_secret     text
);

create table if not exists app_meta.role(
  role_id  serial primary key,
  code     text unique not null,
  title    text not null
);

create table if not exists app_meta.user_role(
  user_id  bigint references app_meta.user_account(user_id) on delete cascade,
  role_id  int    references app_meta.role(role_id)         on delete cascade,
  primary key(user_id, role_id)
);

create table if not exists app_meta.session_log(
  session_id  text primary key,
  user_id     bigint references app_meta.user_account(user_id) on delete set null,
  login_at    timestamptz not null default now(),
  logout_at   timestamptz,
  ip          inet,
  user_agent  text
);

create table if not exists app_meta.notification(
  notification_id bigserial primary key,
  user_id         bigint references app_meta.user_account(user_id) on delete cascade,
  message         text not null,
  is_read         boolean not null default false,
  created_at      timestamptz not null default now()
);

create or replace function app_meta.tg_updated_at() returns trigger
language plpgsql as $$
begin
  new.updated_at = now();
  return new;
end $$;

drop trigger if exists user_account_updated_at on app_meta.user_account;
create trigger user_account_updated_at
before update on app_meta.user_account
for each row execute function app_meta.tg_updated_at();

insert into app_meta.role(code,title) values
  ('admin','Administrátor'),('user','Uživatel')
on conflict do nothing;

create or replace view app_meta.v_user_roles as
select u.user_id, u.email, u.display_name, u.is_active,
       string_agg(r.code, ',') as roles
from app_meta.user_account u
left join app_meta.user_role ur on ur.user_id=u.user_id
left join app_meta.role r on r.role_id=ur.role_id
group by u.user_id, u.email, u.display_name, u.is_active;
