DO $$
    DECLARE
        fid VARCHAR;
        keepID BIGINT;
        oldIDs users.id%type;
BEGIN
    FOR fid IN (select facebook_id as fid from users group by facebook_id having count(*)>1)
    LOOP
        keepID := (select MAX(id) from users where facebook_id = fid);
        select id into oldIDs from users where facebook_id = fid EXCEPT select keepID;

        BEGIN
            update verbals set "user" = keepID where "user" in (select oldIDs);
        EXCEPTION
            WHEN unique_violation THEN
                delete from verbals where "user" in (select oldIDs);
        END;

        BEGIN
            update results set "user" = keepID where "user" in (select oldIDs);
        EXCEPTION
            WHEN unique_violation THEN
                delete from results where "user" in (select oldIDs);
        END;

        delete from users where id in (select oldIDs);
    END LOOP;

    IF NOT EXISTS
        (select constraint_name
          from information_schema.constraint_column_usage
          where table_name = 'users' and constraint_name = 'unique_user_facebook_id') THEN
        ALTER TABLE "users" ADD CONSTRAINT "unique_user_facebook_id" UNIQUE("facebook_id");
    END IF;
    RETURN;
END;
$$ LANGUAGE plpgsql;
