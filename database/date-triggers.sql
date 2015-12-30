-- Add "created_at" and "updated_at" columns
ALTER TABLE "users" ADD COLUMN created_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "sessions" ADD COLUMN created_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "tribes" ADD COLUMN created_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "locations" ADD COLUMN created_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "workouts" ADD COLUMN created_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "verbals" ADD COLUMN created_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "results" ADD COLUMN created_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "events" ADD COLUMN created_at timestamp NOT NULL DEFAULT now();

ALTER TABLE "users" ADD COLUMN updated_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "sessions" ADD COLUMN updated_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "tribes" ADD COLUMN updated_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "locations" ADD COLUMN updated_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "workouts" ADD COLUMN updated_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "verbals" ADD COLUMN updated_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "results" ADD COLUMN updated_at timestamp NOT NULL DEFAULT now();
ALTER TABLE "events" ADD COLUMN updated_at timestamp NOT NULL DEFAULT now();

-- Define the function to set the "updated_at" column
CREATE OR REPLACE FUNCTION set_updated_at_column() 
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW; 
END;
$$ language 'plpgsql';

-- Create Triggers
CREATE TRIGGER set_users_updated_at BEFORE UPDATE ON "users" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_sessions_updated_at BEFORE UPDATE ON "sessions" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_tribes_updated_at BEFORE UPDATE ON "tribes" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_locations_updated_at BEFORE UPDATE ON "locations" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_workouts_updated_at BEFORE UPDATE ON "workouts" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_verbals_updated_at BEFORE UPDATE ON "verbals" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_results_updated_at BEFORE UPDATE ON "results" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_events_updated_at BEFORE UPDATE ON "events" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
