import pytest

from zara.config import ConfigError, ZaraConfig


def write_config(tmp_path, body):
    config_path = tmp_path / "config.toml"
    config_path.write_text(body)
    return config_path


def test_tasks_defaults_are_gated_off(tmp_path):
    config_path = write_config(tmp_path, "[tasks]\n")
    config = ZaraConfig(str(config_path))
    assert config.get_tasks_config() == {
        "enabled": False,
        "max_concurrent": 2,
        "max_task_steps": 20,
        "wall_clock_minutes": 30.0,
        "step_log_chars": 2000,
    }


def test_tasks_values_are_preserved(tmp_path):
    config_path = write_config(
        tmp_path,
        "[tasks]\nenabled = true\nmax_concurrent = 3\nmax_task_steps = 7\n"
        "wall_clock_minutes = 1.5\nstep_log_chars = 512\n",
    )
    config = ZaraConfig(str(config_path))
    assert config.get_tasks_config() == {
        "enabled": True,
        "max_concurrent": 3,
        "max_task_steps": 7,
        "wall_clock_minutes": 1.5,
        "step_log_chars": 512,
    }


@pytest.mark.parametrize(
    "setting",
    [
        "enabled = 1",
        'enabled = "yes"',
        "max_concurrent = true",
        "max_concurrent = 0",
        "max_concurrent = 17",
        "max_concurrent = 1.5",
        "max_task_steps = false",
        "max_task_steps = 0",
        "max_task_steps = 1001",
        "wall_clock_minutes = true",
        "wall_clock_minutes = 0",
        "wall_clock_minutes = -1.0",
        "wall_clock_minutes = nan",
        "wall_clock_minutes = inf",
        "step_log_chars = true",
        "step_log_chars = 0",
        'step_log_chars = "big"',
    ],
)
def test_tasks_settings_are_validated(tmp_path, setting):
    config_path = write_config(tmp_path, f"[tasks]\n{setting}\n")
    with pytest.raises(ConfigError, match="tasks"):
        ZaraConfig(str(config_path))


def test_tasks_section_must_be_a_table(tmp_path):
    config_path = write_config(tmp_path, 'tasks = "enabled"\n')
    with pytest.raises(ConfigError, match="tasks"):
        ZaraConfig(str(config_path))
