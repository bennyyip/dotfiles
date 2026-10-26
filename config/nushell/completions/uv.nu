module completions {

  def "nu-complete uv python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv color" [] {
    [ "auto" "always" "never" ]
  }

  # An extremely fast Python package manager.
  export extern uv [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    --version(-V)             # Display the uv version
  ]

  def "nu-complete uv auth python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv auth python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv auth color" [] {
    [ "auto" "always" "never" ]
  }

  # Manage authentication
  export extern "uv auth" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv auth python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv auth python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv auth color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv auth login keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv auth login python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv auth login python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv auth login color" [] {
    [ "auto" "always" "never" ]
  }

  # Login to a service
  export extern "uv auth login" [
    --username(-u): string    # The username to use for the service
    --password: string        # The password to use for the service
    --token(-t): string       # The token to use for the service
    --keyring-provider: string@"nu-complete uv auth login keyring_provider"
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv auth login python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv auth login python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv auth login color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    service: string           # The domain or URL of the service to log into
  ]

  def "nu-complete uv auth logout keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv auth logout python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv auth logout python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv auth logout color" [] {
    [ "auto" "always" "never" ]
  }

  # Logout of a service
  export extern "uv auth logout" [
    --username(-u): string    # The username to logout
    --keyring-provider: string@"nu-complete uv auth logout keyring_provider"
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv auth logout python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv auth logout python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv auth logout color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    service: string           # The domain or URL of the service to logout from
  ]

  def "nu-complete uv auth token keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv auth token python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv auth token python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv auth token color" [] {
    [ "auto" "always" "never" ]
  }

  # Show the authentication token for a service
  export extern "uv auth token" [
    --username(-u): string    # The username to lookup
    --keyring-provider: string@"nu-complete uv auth token keyring_provider"
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv auth token python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv auth token python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv auth token color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    service: string           # The domain or URL of the service to lookup
  ]

  def "nu-complete uv auth dir python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv auth dir python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv auth dir color" [] {
    [ "auto" "always" "never" ]
  }

  # Show the path to the uv credentials directory
  export extern "uv auth dir" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv auth dir python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv auth dir python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv auth dir color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv auth helper protocol" [] {
    [ "bazel" ]
  }

  def "nu-complete uv auth helper python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv auth helper python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv auth helper color" [] {
    [ "auto" "always" "never" ]
  }

  # Act as a credential helper for external tools
  export extern "uv auth helper" [
    --protocol: string@"nu-complete uv auth helper protocol" # The credential helper protocol to use
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv auth helper python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv auth helper python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv auth helper color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv auth helper get python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv auth helper get python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv auth helper get color" [] {
    [ "auto" "always" "never" ]
  }

  # Retrieve credentials for a URI
  export extern "uv auth helper get" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv auth helper get python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv auth helper get python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv auth helper get color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv run index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv run keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv run resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv run prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv run fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv run link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv run python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv run python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv run python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv run color" [] {
    [ "auto" "always" "never" ]
  }

  # Run a command or script
  export extern "uv run" [
    --extra: string           # Include optional dependencies from the specified extra name
    --all-extras              # Include all optional dependencies
    --no-extra: string        # Exclude the specified optional dependencies, if `--all-extras` is supplied
    --no-all-extras
    --dev                     # Include the development dependency group [env: UV_DEV=]
    --no-dev                  # Disable the development dependency group [env: UV_NO_DEV=]
    --only-dev                # Only include the development dependency group
    --group: string           # Include dependencies from the specified dependency group
    --no-group: string        # Disable the specified dependency group [env: `UV_NO_GROUP`=]
    --no-default-groups       # Ignore the default dependency groups
    --only-group: string      # Only include dependencies from the specified dependency group
    --all-groups              # Include dependencies from all dependency groups
    --module(-m)              # Run a Python module
    --editable                # Install any non-editable dependencies, including the project and any workspace members, as editable
    --no-editable             # Install any editable dependencies, including the project and any workspace members, as non-editable [env: UV_NO_EDITABLE=]
    --no-editable-package: string # Install the specified editable packages as non-editable
    --inexact                 # Do not remove extraneous packages present in the environment
    --exact                   # Perform an exact sync, removing extraneous packages
    --env-file: path          # Load environment variables from a `.env` file
    --no-env-file             # Avoid reading environment variables from a `.env` file [env: UV_NO_ENV_FILE=]
    --with(-w): string        # Run with the given packages installed
    --with-editable: path     # Run with the given packages installed in editable mode
    --with-requirements: path # Run with the packages listed in the given files
    --isolated                # Run the command in an isolated virtual environment [env: UV_ISOLATED=]
    --active                  # Prefer the active virtual environment over the project's virtual environment
    --no-active               # Prefer project's virtual environment over an active environment
    --no-sync                 # Avoid syncing the virtual environment [env: UV_NO_SYNC=]
    --locked                  # Assert that the `uv.lock` will remain unchanged [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Run without updating the `uv.lock` file [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --script(-s)              # Run the given path as a Python script
    --gui-script              # Run the given path as a Python GUI script
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv run index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv run keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv run resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv run prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv run fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv run link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --all-packages            # Run the command with all workspace members installed
    --package: string         # Run the command in a specific package in the workspace
    --no-project              # Avoid discovering the project or workspace
    --python(-p): string      # The Python interpreter to use for the run environment.
    --show-resolution         # Whether to show resolver and installer output from any environment modifications [env: UV_SHOW_RESOLUTION=]
    --max-recursion-depth: string # Number of times that `uv run` will allow recursive invocations
    --python-platform: string@"nu-complete uv run python_platform" # The platform for which requirements should be installed
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv run python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv run python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv run color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv init vcs" [] {
    [ "git" "none" ]
  }

  def "nu-complete uv init build_backend" [] {
    [ "uv" "hatch" "flit" "pdm" "poetry" "setuptools" "maturin" "scikit" ]
  }

  def "nu-complete uv init author_from" [] {
    [ "auto" "git" "none" ]
  }

  def "nu-complete uv init python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv init python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv init color" [] {
    [ "auto" "always" "never" ]
  }

  # Create a new project
  export extern "uv init" [
    --name: string            # The name of the project
    --bare                    # Only create a `pyproject.toml`
    --virtual                 # Create a virtual project, rather than a package
    --package                 # Set up the project to be built as a Python package
    --no-package              # Do not set up the project to be built as a Python package
    --app                     # Create a project for an application
    --lib                     # Create a project for a library
    --script                  # Create a script
    --description: string     # Set the project description
    --no-description          # Disable the description for the project
    --vcs: string@"nu-complete uv init vcs" # Initialize a version control system for the project
    --build-backend: string@"nu-complete uv init build_backend" # Initialize a build-backend of choice for the project
    --backend                 # Invalid option name for build backend
    --no-readme               # Do not create a `README.md` file
    --author-from: string@"nu-complete uv init author_from" # Fill in the `authors` field in the `pyproject.toml`
    --no-pin-python           # Do not create a `.python-version` file for the project
    --pin-python              # Create a `.python-version` file for the project
    --no-workspace            # Avoid discovering a workspace and create a standalone project
    --python(-p): string      # The Python interpreter to use to determine the minimum supported Python version.
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv init python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv init python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv init color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    path?: path               # The path to use for the project/script
  ]

  def "nu-complete uv add bounds" [] {
    [ "lower" "major" "minor" "exact" ]
  }

  def "nu-complete uv add index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv add keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv add resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv add prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv add fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv add link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv add python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv add python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv add color" [] {
    [ "auto" "always" "never" ]
  }

  # Add dependencies to the project
  export extern "uv add" [
    --requirements(-r): path  # Add the packages listed in the given files
    --constraints(-c): path   # Constrain versions using the given requirements files [env: `UV_CONSTRAINT`=]
    --marker(-m): string      # Apply this marker to all added packages
    --dev                     # Add the requirements to the development dependency group [env: UV_DEV=]
    --optional: string        # Add the requirements to the package's optional dependencies for the specified extra
    --group: string           # Add the requirements to the specified dependency group
    --editable                # Add the requirements as editable
    --no-editable             # Don't add the requirements as editable [env: UV_NO_EDITABLE=]
    --no-editable-package: string # Don't add the specified requirements as editable
    --raw                     # Add a dependency as provided
    --bounds: string@"nu-complete uv add bounds" # The kind of version specifier to use when adding dependencies
    --rev: string             # Commit to use when adding a dependency from Git
    --tag: string             # Tag to use when adding a dependency from Git
    --branch: string          # Branch to use when adding a dependency from Git
    --lfs                     # Whether to use Git LFS when adding a dependency from Git
    --extra: string           # Extras to enable for the dependency
    --no-sync                 # Avoid syncing the virtual environment [env: UV_NO_SYNC=]
    --locked                  # Assert that the `uv.lock` will remain unchanged [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Add dependencies without re-locking the project [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --active                  # Prefer the active virtual environment over the project's virtual environment
    --no-active               # Prefer project's virtual environment over an active environment
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv add index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv add keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv add resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv add prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv add fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv add link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --package: string         # Add the dependency to a specific package in the workspace
    --script: path            # Add the dependency to the specified Python script, rather than to a project
    --python(-p): string      # The Python interpreter to use for resolving and syncing.
    --workspace               # Add the dependency as a workspace member
    --no-workspace            # Don't add the dependency as a workspace member
    --no-install-project      # Do not install the current project [env: UV_NO_INSTALL_PROJECT=]
    --only-install-project    # Only install the current project
    --no-install-workspace    # Do not install any workspace members, including the current project [env: UV_NO_INSTALL_WORKSPACE=]
    --only-install-workspace  # Only install workspace members, including the current project
    --no-install-local        # Do not install local path dependencies [env: UV_NO_INSTALL_LOCAL=]
    --only-install-local      # Only install local path dependencies
    --no-install-package: string # Do not install the given package(s)
    --only-install-package: string # Only install the given package(s)
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv add python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv add python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv add color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...packages: string       # The packages to add, as PEP 508 requirements (e.g., `ruff==0.5.0`)
  ]

  def "nu-complete uv remove index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv remove keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv remove resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv remove prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv remove fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv remove link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv remove python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv remove python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv remove color" [] {
    [ "auto" "always" "never" ]
  }

  # Remove dependencies from the project
  export extern "uv remove" [
    --dev                     # Remove the packages from the development dependency group [env: UV_DEV=]
    --optional: string        # Remove the packages from the project's optional dependencies for the specified extra
    --group: string           # Remove the packages from the specified dependency group
    --no-sync                 # Avoid syncing the virtual environment after re-locking the project [env: UV_NO_SYNC=]
    --active                  # Prefer the active virtual environment over the project's virtual environment
    --no-active               # Prefer project's virtual environment over an active environment
    --locked                  # Assert that the `uv.lock` will remain unchanged [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Remove dependencies without re-locking the project [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv remove index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv remove keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv remove resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv remove prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv remove fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv remove link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --package: string         # Remove the dependencies from a specific package in the workspace
    --script: path            # Remove the dependency from the specified Python script, rather than from a project
    --python(-p): string      # The Python interpreter to use for resolving and syncing.
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv remove python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv remove python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv remove color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...packages: string       # The names of the dependencies to remove (e.g., `ruff`)
  ]

  def "nu-complete uv version bump" [] {
    [ "major" "minor" "patch" "stable" "alpha" "beta" "rc" "post" "dev" ]
  }

  def "nu-complete uv version output_format" [] {
    [ "text" "json" ]
  }

  def "nu-complete uv version index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv version keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv version resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv version prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv version fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv version link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv version python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv version python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv version color" [] {
    [ "auto" "always" "never" ]
  }

  # Read or update the project's version
  export extern "uv version" [
    --bump: string@"nu-complete uv version bump" # Update the project version using the given semantics
    --dry-run                 # Don't write a new version to the `pyproject.toml`
    --short                   # Only show the version
    --output-format: string@"nu-complete uv version output_format" # The format of the output
    --no-sync                 # Avoid syncing the virtual environment after re-locking the project [env: UV_NO_SYNC=]
    --active                  # Prefer the active virtual environment over the project's virtual environment
    --no-active               # Prefer project's virtual environment over an active environment
    --locked                  # Assert that the `uv.lock` will remain unchanged [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Update the version without re-locking the project [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv version index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv version keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv version resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv version prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv version fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv version link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --package: string         # Update the version of a specific package in the workspace
    --python(-p): string      # The Python interpreter to use for resolving and syncing.
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv version python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv version python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv version color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    value?: string            # Set the project version to this value
  ]

  def "nu-complete uv sync output_format" [] {
    [ "text" "json" ]
  }

  def "nu-complete uv sync index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv sync keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv sync resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv sync prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv sync fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv sync link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv sync python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv sync python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv sync python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv sync color" [] {
    [ "auto" "always" "never" ]
  }

  # Update the project's environment
  export extern "uv sync" [
    --extra: string           # Include optional dependencies from the specified extra name
    --output-format: string@"nu-complete uv sync output_format" # Select the output format
    --all-extras              # Include all optional dependencies
    --no-extra: string        # Exclude the specified optional dependencies, if `--all-extras` is supplied
    --no-all-extras
    --dev                     # Include the development dependency group [env: UV_DEV=]
    --no-dev                  # Disable the development dependency group [env: UV_NO_DEV=]
    --only-dev                # Only include the development dependency group
    --group: string           # Include dependencies from the specified dependency group
    --no-group: string        # Disable the specified dependency group [env: `UV_NO_GROUP`=]
    --no-default-groups       # Ignore the default dependency groups
    --only-group: string      # Only include dependencies from the specified dependency group
    --all-groups              # Include dependencies from all dependency groups
    --editable                # Install any non-editable dependencies, including the project and any workspace members, as editable
    --no-editable             # Install any editable dependencies, including the project and any workspace members, as non-editable [env: UV_NO_EDITABLE=]
    --no-editable-package: string # Install the specified editable packages as non-editable
    --inexact                 # Do not remove extraneous packages present in the environment
    --exact                   # Perform an exact sync, removing extraneous packages
    --active                  # Sync dependencies to the active virtual environment
    --no-active               # Prefer project's virtual environment over an active environment
    --no-install-project      # Do not install the current project [env: UV_NO_INSTALL_PROJECT=]
    --only-install-project    # Only install the current project
    --no-install-workspace    # Do not install any workspace members, including the root project [env: UV_NO_INSTALL_WORKSPACE=]
    --only-install-workspace  # Only install workspace members, including the root project
    --no-install-local        # Do not install local path dependencies [env: UV_NO_INSTALL_LOCAL=]
    --only-install-local      # Only install local path dependencies
    --no-install-package: string # Do not install the given package(s)
    --only-install-package: string # Only install the given package(s)
    --locked                  # Assert that the `uv.lock` will remain unchanged [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Sync without updating the `uv.lock` file [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --dry-run                 # Perform a dry run, without writing the lockfile or modifying the project environment
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv sync index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv sync keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv sync resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv sync prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv sync fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv sync link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --all-packages            # Sync all packages in the workspace
    --package: string         # Sync for specific packages in the workspace
    --script: path            # Sync the environment for a Python script, rather than the current project
    --python(-p): string      # The Python interpreter to use for the project environment.
    --python-platform: string@"nu-complete uv sync python_platform" # The platform for which requirements should be installed
    --check                   # Check if the Python environment is synchronized with the project
    --no-check
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv sync python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv sync python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv sync color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv lock index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv lock keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv lock resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv lock prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv lock fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv lock link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv lock python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv lock python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv lock color" [] {
    [ "auto" "always" "never" ]
  }

  # Update the project's lockfile
  export extern "uv lock" [
    --check                   # Check if the lockfile is up-to-date
    --locked                  # Check if the lockfile is up-to-date [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --check-exists            # Assert that a `uv.lock` exists without checking if it is up-to-date [env: UV_FROZEN=]
    --frozen                  # Equivalent to `--check-exists`
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --dry-run                 # Perform a dry run, without writing the lockfile
    --script: path            # Lock the specified Python script, rather than the current project
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --index-strategy: string@"nu-complete uv lock index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv lock keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv lock resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv lock prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv lock fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv lock link_mode" # The method to use when installing packages from the global cache
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --python(-p): string      # The Python interpreter to use during resolution.
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv lock python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv lock python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv lock color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv upgrade index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv upgrade keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv upgrade python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv upgrade python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv upgrade color" [] {
    [ "auto" "always" "never" ]
  }

  # Upgrade a dependency in the project
  export extern "uv upgrade" [
    --exclude: string         # Exclude the named package from upgrades
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --index-strategy: string@"nu-complete uv upgrade index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv upgrade keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv upgrade python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv upgrade python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv upgrade color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...packages: string       # The packages to upgrade
  ]

  def "nu-complete uv export format" [] {
    [ "requirements.txt" "pylock.toml" "cyclonedx1.5" ]
  }

  def "nu-complete uv export index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv export keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv export resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv export prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv export fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv export link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv export python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv export python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv export color" [] {
    [ "auto" "always" "never" ]
  }

  # Export the project's lockfile to an alternate format
  export extern "uv export" [
    --format: string@"nu-complete uv export format" # The format to which `uv.lock` should be exported
    --all-packages            # Export the entire workspace
    --package: string         # Export the dependencies for specific packages in the workspace
    --prune: string           # Prune the given package from the dependency tree
    --extra: string           # Include optional dependencies from the specified extra name
    --all-extras              # Include all optional dependencies
    --no-extra: string        # Exclude the specified optional dependencies, if `--all-extras` is supplied
    --no-all-extras
    --dev                     # Include the development dependency group [env: UV_DEV=]
    --no-dev                  # Disable the development dependency group [env: UV_NO_DEV=]
    --only-dev                # Only include the development dependency group
    --group: string           # Include dependencies from the specified dependency group
    --no-group: string        # Disable the specified dependency group [env: `UV_NO_GROUP`=]
    --no-default-groups       # Ignore the default dependency groups
    --only-group: string      # Only include dependencies from the specified dependency group
    --all-groups              # Include dependencies from all dependency groups
    --no-annotate             # Exclude comment annotations indicating the source of each package
    --annotate
    --no-header               # Exclude the comment header at the top of the generated output file
    --header
    --emit-index-url          # Include `--index-url` and `--extra-index-url` entries in the generated output file
    --no-emit-index-url
    --emit-find-links         # Include `--find-links` entries in the generated output file
    --no-emit-find-links
    --editable                # Export any non-editable dependencies, including the project and any workspace members, as editable
    --no-editable             # Export any editable dependencies, including the project and any workspace members, as non-editable [env: UV_NO_EDITABLE=]
    --no-editable-package: string # Export the specified editable packages as non-editable
    --hashes                  # Include hashes for all dependencies
    --no-hashes               # Omit hashes in the generated output
    --output-file(-o): path   # Write the exported requirements to the given file
    --batch: path             # Export multiple selections from a TOML manifest containing `[[export]]` entries
    --no-emit-project         # Do not emit the current project
    --only-emit-project       # Only emit the current project
    --no-emit-workspace       # Do not emit any workspace members, including the root project
    --only-emit-workspace     # Only emit workspace members, including the root project
    --no-emit-local           # Do not include local path dependencies in the exported requirements
    --only-emit-local         # Only include local path dependencies in the exported requirements
    --no-emit-package: string # Do not emit the given package(s)
    --only-emit-package: string # Only emit the given package(s)
    --locked                  # Assert that the `uv.lock` will remain unchanged [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Do not update the `uv.lock` before exporting [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --index-strategy: string@"nu-complete uv export index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv export keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv export resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv export prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv export fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv export link_mode" # The method to use when installing packages from the global cache
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --script: path            # Export the dependencies for the specified PEP 723 Python script, rather than the current project
    --python(-p): string      # The Python interpreter to use during resolution.
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv export python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv export python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv export color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv tree format" [] {
    [ "text" "json" ]
  }

  def "nu-complete uv tree index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv tree keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv tree resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv tree prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv tree fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv tree link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv tree python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv tree python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tree python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tree color" [] {
    [ "auto" "always" "never" ]
  }

  # Display the project's dependency tree
  export extern "uv tree" [
    --universal               # Show a platform-independent dependency tree
    --format: string@"nu-complete uv tree format" # The format in which to display the dependency graph
    --depth(-d): string       # Maximum display depth of the dependency tree
    --prune: string           # Prune the given package from the display of the dependency tree
    --package: string         # Display only the specified packages
    --no-dedupe               # Do not de-duplicate repeated dependencies. Usually, when a package has already displayed its dependencies, further occurrences will not re-display its dependencies, and will include a (*) to indicate it has already been shown. This flag will cause those duplicates to be repeated
    --invert                  # Show the reverse dependencies for the given package. This flag will invert the tree and display the packages that depend on the given package
    --outdated                # Show the latest available version of each package in the tree
    --show-sizes              # Show compressed wheel sizes for packages in the tree
    --dev                     # Include the development dependency group [env: UV_DEV=]
    --no-dev                  # Disable the development dependency group [env: UV_NO_DEV=]
    --only-dev                # Only include the development dependency group
    --group: string           # Include dependencies from the specified dependency group
    --no-group: string        # Disable the specified dependency group [env: `UV_NO_GROUP`=]
    --no-default-groups       # Ignore the default dependency groups
    --only-group: string      # Only include dependencies from the specified dependency group
    --all-groups              # Include dependencies from all dependency groups
    --locked                  # Assert that the `uv.lock` will remain unchanged [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Display the requirements without locking the project [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --index-strategy: string@"nu-complete uv tree index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv tree keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv tree resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv tree prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv tree fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv tree link_mode" # The method to use when installing packages from the global cache
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --script: path            # Show the dependency tree the specified PEP 723 Python script, rather than the current project
    --python-version: string  # The Python version to use when filtering the tree
    --python-platform: string@"nu-complete uv tree python_platform" # The platform to use when filtering the tree
    --python(-p): string      # The Python interpreter to use for locking and filtering.
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tree python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tree python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tree color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv format python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv format python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv format color" [] {
    [ "auto" "always" "never" ]
  }

  # Format Python code in the project
  export extern "uv format" [
    --check                   # Check if files are formatted without applying changes
    --diff                    # Show a diff of formatting changes without applying them
    --version: string         # The version of Ruff to use for formatting
    --exclude-newer: string   # Limit candidate Ruff versions to those released prior to the given date
    --no-project              # Avoid discovering a project or workspace
    --show-version            # Display the version of Ruff that will be used for formatting
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv format python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv format python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv format color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...extra_args: string     # Additional arguments to pass to Ruff
  ]

  def "nu-complete uv check index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv check keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv check resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv check prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv check fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv check link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv check python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv check python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv check color" [] {
    [ "auto" "always" "never" ]
  }

  # Run checks on the project
  export extern "uv check" [
    --fix                     # Apply safe fixes to resolve type-checking errors
    --all-packages            # Check all packages in the workspace
    --package: string         # Check specific packages in the workspace
    --script: path            # Run checks for the specified PEP 723 Python script, rather than the current project
    --extra: string           # Include optional dependencies from the specified extra name
    --all-extras              # Include all optional dependencies
    --no-extra: string        # Exclude the specified optional dependencies, if `--all-extras` is supplied
    --no-all-extras
    --dev                     # Include the development dependency group [env: UV_DEV=]
    --no-dev                  # Disable the development dependency group [env: UV_NO_DEV=]
    --only-dev                # Only include the development dependency group
    --group: string           # Include dependencies from the specified dependency group
    --no-group: string        # Disable the specified dependency group [env: `UV_NO_GROUP`=]
    --no-default-groups       # Ignore the default dependency groups
    --only-group: string      # Only include dependencies from the specified dependency group
    --all-groups              # Include dependencies from all dependency groups
    --locked                  # Assert that the `uv.lock` will remain unchanged [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Sync without updating the `uv.lock` file [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --no-sync                 # Avoid syncing the virtual environment [env: UV_NO_SYNC=]
    --no-install-project      # Do not install the current project [env: UV_NO_INSTALL_PROJECT=]
    --isolated                # Run checks without mutating project state [env: UV_ISOLATED=]
    --python(-p): string      # The Python interpreter to use for the project environment
    --ty-version: string      # The version of ty to use for type checking
    --show-version            # Display the version of ty that will be used for type checking
    --show-command            # Display the ty command that will be used for type checking
    --no-project              # Avoid discovering a project or workspace
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv check index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv check keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv check resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv check prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv check fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv check link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv check python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv check python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv check color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv audit output_format" [] {
    [ "text" "json" "sarif" ]
  }

  def "nu-complete uv audit service_format" [] {
    [ "osv" ]
  }

  def "nu-complete uv audit index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv audit keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv audit resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv audit prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv audit fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv audit link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv audit python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv audit python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv audit python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv audit color" [] {
    [ "auto" "always" "never" ]
  }

  # Audit the project's dependencies
  export extern "uv audit" [
    --no-extra: string        # Don't audit the specified optional dependencies
    --no-dev                  # Don't audit the development dependency group [env: UV_NO_DEV=]
    --no-group: string        # Don't audit the specified dependency group [env: `UV_NO_GROUP`=]
    --no-default-groups       # Don't audit dependency groups unless explicitly requested
    --only-group: string      # Only audit dependencies from the specified dependency group
    --only-dev                # Only audit the development dependency group
    --locked                  # Assert that the `uv.lock` will remain unchanged [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Audit the requirements without locking the project [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --offline
    --output-format: string@"nu-complete uv audit output_format" # Select the output format
    --ignore: string          # Ignore a vulnerability by ID
    --ignore-until-fixed: string # Ignore a vulnerability by ID, but only while no fix is available
    --service-format: string@"nu-complete uv audit service_format" # The service format to use for vulnerability lookups
    --service-url: string     # The URL to vulnerability service API endpoint
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --index-strategy: string@"nu-complete uv audit index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv audit keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv audit resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv audit prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv audit fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv audit link_mode" # The method to use when installing packages from the global cache
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --script: path            # Audit the specified PEP 723 Python script, rather than the current project
    --python-version: string  # The Python version to use when auditing
    --python-platform: string@"nu-complete uv audit python_platform" # The platform to use when auditing
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv audit python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv audit python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv audit color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv tool python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool color" [] {
    [ "auto" "always" "never" ]
  }

  # Run and install commands provided by Python packages
  export extern "uv tool" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tool python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tool python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv tool run index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv tool run keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv tool run resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv tool run prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv tool run fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv tool run link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv tool run python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv tool run torch_backend" [] {
    [ "auto" "cpu" "cu132" "cu130" "cu129" "cu128" "cu126" "cu125" "cu124" "cu123" "cu122" "cu121" "cu120" "cu118" "cu117" "cu116" "cu115" "cu114" "cu113" "cu112" "cu111" "cu110" "cu102" "cu101" "cu100" "cu92" "cu91" "cu90" "cu80" "rocm7.2" "rocm7.1" "rocm7.0" "rocm6.4" "rocm6.3" "rocm6.2.4" "rocm6.2" "rocm6.1" "rocm6.0" "rocm5.7" "rocm5.6" "rocm5.5" "rocm5.4.2" "rocm5.4" "rocm5.3" "rocm5.2" "rocm5.1.1" "rocm4.2" "rocm4.1" "rocm4.0.1" "xpu" ]
  }

  def "nu-complete uv tool run generate_shell_completion" [] {
    [ "bash" "elvish" "fish" "nushell" "powershell" "zsh" ]
  }

  def "nu-complete uv tool run python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool run python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool run color" [] {
    [ "auto" "always" "never" ]
  }

  # Run a command provided by a Python package
  export extern "uv tool run" [
    --from: string            # Use the given package to provide the command
    --with(-w): string        # Run with the given packages installed
    --with-editable: path     # Run with the given packages installed in editable mode
    --with-requirements: path # Run with the packages listed in the given files
    --constraints(-c): path   # Constrain versions using the given requirements files [env: `UV_CONSTRAINT`=]
    --build-constraints(-b): path # Constrain build dependencies using the given requirements files when building source distributions [env: `UV_BUILD_CONSTRAINT`=]
    --overrides: path         # Override versions using the given requirements files [env: `UV_OVERRIDE`=]
    --isolated                # Run the tool in an isolated virtual environment, ignoring any already-installed tools [env: UV_ISOLATED=]
    --env-file: path          # Load environment variables from a `.env` file
    --no-env-file             # Avoid reading environment variables from a `.env` file [env: UV_NO_ENV_FILE=]
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv tool run index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv tool run keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv tool run resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv tool run prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv tool run fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv tool run link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --lfs                     # Whether to use Git LFS when adding a dependency from Git
    --python(-p): string      # The Python interpreter to use to build the run environment.
    --show-resolution         # Whether to show resolver and installer output from any environment modifications [env: UV_SHOW_RESOLUTION=]
    --python-platform: string@"nu-complete uv tool run python_platform" # The platform for which requirements should be installed
    --torch-backend: string@"nu-complete uv tool run torch_backend" # The backend to use when fetching packages in the PyTorch ecosystem (e.g., `cpu`, `cu126`, or `auto`)
    --generate-shell-completion: string@"nu-complete uv tool run generate_shell_completion"
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tool run python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tool run python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool run color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv tool uvx index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv tool uvx keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv tool uvx resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv tool uvx prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv tool uvx fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv tool uvx link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv tool uvx python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv tool uvx torch_backend" [] {
    [ "auto" "cpu" "cu132" "cu130" "cu129" "cu128" "cu126" "cu125" "cu124" "cu123" "cu122" "cu121" "cu120" "cu118" "cu117" "cu116" "cu115" "cu114" "cu113" "cu112" "cu111" "cu110" "cu102" "cu101" "cu100" "cu92" "cu91" "cu90" "cu80" "rocm7.2" "rocm7.1" "rocm7.0" "rocm6.4" "rocm6.3" "rocm6.2.4" "rocm6.2" "rocm6.1" "rocm6.0" "rocm5.7" "rocm5.6" "rocm5.5" "rocm5.4.2" "rocm5.4" "rocm5.3" "rocm5.2" "rocm5.1.1" "rocm4.2" "rocm4.1" "rocm4.0.1" "xpu" ]
  }

  def "nu-complete uv tool uvx generate_shell_completion" [] {
    [ "bash" "elvish" "fish" "nushell" "powershell" "zsh" ]
  }

  def "nu-complete uv tool uvx python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool uvx python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool uvx color" [] {
    [ "auto" "always" "never" ]
  }

  # Run a command provided by a Python package.
  export extern "uv tool uvx" [
    --from: string            # Use the given package to provide the command
    --with(-w): string        # Run with the given packages installed
    --with-editable: path     # Run with the given packages installed in editable mode
    --with-requirements: path # Run with the packages listed in the given files
    --constraints(-c): path   # Constrain versions using the given requirements files [env: `UV_CONSTRAINT`=]
    --build-constraints(-b): path # Constrain build dependencies using the given requirements files when building source distributions [env: `UV_BUILD_CONSTRAINT`=]
    --overrides: path         # Override versions using the given requirements files [env: `UV_OVERRIDE`=]
    --isolated                # Run the tool in an isolated virtual environment, ignoring any already-installed tools [env: UV_ISOLATED=]
    --env-file: path          # Load environment variables from a `.env` file
    --no-env-file             # Avoid reading environment variables from a `.env` file [env: UV_NO_ENV_FILE=]
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv tool uvx index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv tool uvx keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv tool uvx resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv tool uvx prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv tool uvx fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv tool uvx link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --lfs                     # Whether to use Git LFS when adding a dependency from Git
    --python(-p): string      # The Python interpreter to use to build the run environment.
    --show-resolution         # Whether to show resolver and installer output from any environment modifications [env: UV_SHOW_RESOLUTION=]
    --python-platform: string@"nu-complete uv tool uvx python_platform" # The platform for which requirements should be installed
    --torch-backend: string@"nu-complete uv tool uvx torch_backend" # The backend to use when fetching packages in the PyTorch ecosystem (e.g., `cpu`, `cu126`, or `auto`)
    --generate-shell-completion: string@"nu-complete uv tool uvx generate_shell_completion"
    --version(-V)             # Display the uvx version
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tool uvx python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tool uvx python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool uvx color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv tool install index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv tool install keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv tool install resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv tool install prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv tool install fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv tool install link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv tool install python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv tool install torch_backend" [] {
    [ "auto" "cpu" "cu132" "cu130" "cu129" "cu128" "cu126" "cu125" "cu124" "cu123" "cu122" "cu121" "cu120" "cu118" "cu117" "cu116" "cu115" "cu114" "cu113" "cu112" "cu111" "cu110" "cu102" "cu101" "cu100" "cu92" "cu91" "cu90" "cu80" "rocm7.2" "rocm7.1" "rocm7.0" "rocm6.4" "rocm6.3" "rocm6.2.4" "rocm6.2" "rocm6.1" "rocm6.0" "rocm5.7" "rocm5.6" "rocm5.5" "rocm5.4.2" "rocm5.4" "rocm5.3" "rocm5.2" "rocm5.1.1" "rocm4.2" "rocm4.1" "rocm4.0.1" "xpu" ]
  }

  def "nu-complete uv tool install python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool install python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool install color" [] {
    [ "auto" "always" "never" ]
  }

  # Install commands provided by a Python package
  export extern "uv tool install" [
    --from: string            # The package to install commands from
    --with(-w): string        # Include the following additional requirements
    --with-requirements: path # Run with the packages listed in the given files
    --editable(-e)            # Install the target package in editable mode, such that changes in the package's source directory are reflected without reinstallation
    --with-editable: path     # Include the given packages in editable mode
    --with-executables-from: string # Install executables from the following packages
    --constraints(-c): path   # Constrain versions using the given requirements files [env: `UV_CONSTRAINT`=]
    --overrides: path         # Override versions using the given requirements files [env: `UV_OVERRIDE`=]
    --excludes: path          # Exclude packages from resolution using the given requirements files [env: `UV_EXCLUDE`=]
    --build-constraints(-b): path # Constrain build dependencies using the given requirements files when building source distributions [env: `UV_BUILD_CONSTRAINT`=]
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv tool install index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv tool install keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv tool install resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv tool install prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv tool install fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv tool install link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --force                   # Force installation of the tool
    --lfs                     # Whether to use Git LFS when adding a dependency from Git
    --python(-p): string      # The Python interpreter to use to build the tool environment.
    --python-platform: string@"nu-complete uv tool install python_platform" # The platform for which requirements should be installed
    --torch-backend: string@"nu-complete uv tool install torch_backend" # The backend to use when fetching packages in the PyTorch ecosystem (e.g., `cpu`, `cu126`, or `auto`)
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tool install python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tool install python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool install color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    package: string           # The package to install commands from
  ]

  def "nu-complete uv tool upgrade python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv tool upgrade index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv tool upgrade keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv tool upgrade resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv tool upgrade prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv tool upgrade fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv tool upgrade link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv tool upgrade python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool upgrade python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool upgrade color" [] {
    [ "auto" "always" "never" ]
  }

  # Upgrade installed tools
  export extern "uv tool upgrade" [
    --all                     # Upgrade all tools
    --python(-p): string      # Upgrade a tool, and specify it to use the given Python interpreter to build its environment. Use with `--all` to apply to all tools.
    --python-platform: string@"nu-complete uv tool upgrade python_platform" # The platform for which requirements should be installed
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv tool upgrade index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv tool upgrade keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv tool upgrade resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv tool upgrade prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv tool upgrade fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-setting-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv tool upgrade link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tool upgrade python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tool upgrade python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool upgrade color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...name: string           # The name of the tool to upgrade, along with an optional version specifier
  ]

  def "nu-complete uv tool list python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool list python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool list color" [] {
    [ "auto" "always" "never" ]
  }

  # List installed tools
  export extern "uv tool list" [
    --show-paths              # Whether to display the path to each tool environment and installed executable
    --show-version-specifiers # Whether to display the version specifier(s) used to install each tool
    --show-with               # Whether to display the additional requirements installed with each tool
    --show-extras             # Whether to display the extra requirements installed with each tool
    --show-python             # Whether to display the Python version associated with each tool
    --outdated                # List outdated tools
    --no-outdated
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --python-preference: string@"nu-complete uv tool list python_preference"
    --no-python-downloads
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --python-fetch: string@"nu-complete uv tool list python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool list color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv tool audit output_format" [] {
    [ "text" "json" "sarif" ]
  }

  def "nu-complete uv tool audit service_format" [] {
    [ "osv" ]
  }

  def "nu-complete uv tool audit python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool audit python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool audit color" [] {
    [ "auto" "always" "never" ]
  }

  # Audit installed tools and their dependencies
  export extern "uv tool audit" [
    --all                     # Audit all installed tools
    --offline
    --output-format: string@"nu-complete uv tool audit output_format" # Select the output format
    --ignore: string          # Ignore a vulnerability by ID
    --ignore-until-fixed: string # Ignore a vulnerability by ID, but only while no fix is available
    --service-format: string@"nu-complete uv tool audit service_format" # The service format to use for vulnerability lookups
    --service-url: string     # The URL to vulnerability service API endpoint
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tool audit python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tool audit python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool audit color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...name: string           # The names of the installed tools to audit
  ]

  def "nu-complete uv tool uninstall python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool uninstall python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool uninstall color" [] {
    [ "auto" "always" "never" ]
  }

  # Uninstall a tool
  export extern "uv tool uninstall" [
    --all                     # Uninstall all tools
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tool uninstall python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tool uninstall python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool uninstall color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...name: string           # The name of the tool to uninstall
  ]

  def "nu-complete uv tool update-shell python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool update-shell python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool update-shell color" [] {
    [ "auto" "always" "never" ]
  }

  # Ensure that the tool executable directory is on the `PATH`
  export extern "uv tool update-shell" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tool update-shell python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tool update-shell python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool update-shell color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv tool dir python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv tool dir python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv tool dir color" [] {
    [ "auto" "always" "never" ]
  }

  # Show the path to the uv tools directory
  export extern "uv tool dir" [
    --bin                     # Show the directory into which `uv tool` will install executables.
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv tool dir python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv tool dir python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv tool dir color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv python python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv python color" [] {
    [ "auto" "always" "never" ]
  }

  # Manage Python versions and installations
  export extern "uv python" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv python color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv python list output_format" [] {
    [ "text" "json" ]
  }

  def "nu-complete uv python list python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python list python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv python list color" [] {
    [ "auto" "always" "never" ]
  }

  # List the available Python installations
  export extern "uv python list" [
    --all-versions            # List all Python versions, including old patch versions
    --all-platforms           # List Python downloads for all platforms
    --all-arches              # List Python downloads for all architectures
    --only-installed          # Only show installed Python versions
    --only-downloads          # Only show available Python downloads
    --show-urls               # Show the URLs of available Python downloads
    --output-format: string@"nu-complete uv python list output_format" # Select the output format
    --python-downloads-json-url: string # URL pointing to JSON of custom Python installations
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python list python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python list python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv python list color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    request?: string          # A Python request to filter by
  ]

  def "nu-complete uv python install python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python install python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv python install color" [] {
    [ "auto" "always" "never" ]
  }

  # Download and install Python versions
  export extern "uv python install" [
    --install-dir(-i): path   # The directory to store the Python installation in
    --bin                     # Install a Python executable into the `bin` directory
    --no-bin                  # Do not install a Python executable into the `bin` directory
    --registry                # Register the Python installation in the Windows registry
    --no-registry             # Do not register the Python installation in the Windows registry
    --mirror: string          # Set the URL to use as the source for downloading Python installations
    --pypy-mirror: string     # Set the URL to use as the source for downloading PyPy installations
    --graalpy-mirror: string  # Set the URL to use as the source for downloading GraalPy installations
    --pyodide-mirror: string  # Set the URL to use as the source for downloading Pyodide installations
    --python-downloads-json-url: string # URL pointing to JSON of custom Python installations
    --reinstall(-r)           # Reinstall the requested Python version, if it's already installed
    --force(-f)               # Replace existing Python executables during installation
    --upgrade(-U)             # Upgrade existing Python installations to the latest patch version
    --default                 # Use as the default Python version
    --compile-bytecode        # Compile Python's standard library to bytecode after installation
    --no-compile-bytecode
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python install python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python install python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv python install color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...targets: string        # The Python version(s) to install
  ]

  def "nu-complete uv python upgrade python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python upgrade python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv python upgrade color" [] {
    [ "auto" "always" "never" ]
  }

  # Upgrade installed Python versions
  export extern "uv python upgrade" [
    --install-dir(-i): path   # The directory Python installations are stored in
    --mirror: string          # Set the URL to use as the source for downloading Python installations
    --pypy-mirror: string     # Set the URL to use as the source for downloading PyPy installations
    --graalpy-mirror: string  # Set the URL to use as the source for downloading GraalPy installations
    --pyodide-mirror: string  # Set the URL to use as the source for downloading Pyodide installations
    --reinstall(-r)           # Reinstall the latest Python patch, if it's already installed
    --python-downloads-json-url: string # URL pointing to JSON of custom Python installations
    --compile-bytecode        # Compile Python's standard library to bytecode after installation
    --no-compile-bytecode
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python upgrade python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python upgrade python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv python upgrade color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...targets: string        # The Python minor version(s) to upgrade
  ]

  def "nu-complete uv python find python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python find python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv python find color" [] {
    [ "auto" "always" "never" ]
  }

  # Search for a Python installation
  export extern "uv python find" [
    --no-project              # Avoid discovering a project or workspace
    --system                  # Only find system Python interpreters
    --no-system
    --script: path            # Find the environment for a Python script, rather than the current project
    --show-version            # Show the Python version that would be used instead of the path to the interpreter
    --resolve-links           # Resolve symlinks in the output path
    --python-downloads-json-url: string # URL pointing to JSON of custom Python installations
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python find python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python find python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv python find color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    request?: string          # The Python request
  ]

  def "nu-complete uv python pin python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python pin python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv python pin color" [] {
    [ "auto" "always" "never" ]
  }

  # Pin to a specific Python version
  export extern "uv python pin" [
    --resolved                # Write the resolved Python interpreter path instead of the request
    --no-resolved
    --no-project              # Avoid validating the Python pin is compatible with the project or workspace
    --global                  # Update the global Python version pin
    --rm                      # Remove the Python version pin
    --python-downloads-json-url: string # URL pointing to JSON of custom Python installations
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python pin python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python pin python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv python pin color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    request?: string          # The Python version request
  ]

  def "nu-complete uv python dir python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python dir python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv python dir color" [] {
    [ "auto" "always" "never" ]
  }

  # Show the uv Python installation directory
  export extern "uv python dir" [
    --bin                     # Show the directory into which `uv python` will install Python executables.
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python dir python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python dir python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv python dir color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv python uninstall python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python uninstall python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv python uninstall color" [] {
    [ "auto" "always" "never" ]
  }

  # Uninstall Python versions
  export extern "uv python uninstall" [
    --install-dir(-i): path   # The directory where the Python was installed
    --all                     # Uninstall all managed Python versions
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python uninstall python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python uninstall python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv python uninstall color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...targets: string        # The Python version(s) to uninstall
  ]

  def "nu-complete uv python update-shell python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv python update-shell python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv python update-shell color" [] {
    [ "auto" "always" "never" ]
  }

  # Ensure that the Python executable directory is on the `PATH`
  export extern "uv python update-shell" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv python update-shell python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv python update-shell python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv python update-shell color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv pip python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip color" [] {
    [ "auto" "always" "never" ]
  }

  # Manage Python packages with a pip-compatible interface
  export extern "uv pip" [
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv pip compile index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv pip compile keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv pip compile resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv pip compile prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv pip compile fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv pip compile link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv pip compile format" [] {
    [ "requirements.txt" "pylock.toml" ]
  }

  def "nu-complete uv pip compile annotation_style" [] {
    [ "line" "split" ]
  }

  def "nu-complete uv pip compile python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv pip compile torch_backend" [] {
    [ "auto" "cpu" "cu132" "cu130" "cu129" "cu128" "cu126" "cu125" "cu124" "cu123" "cu122" "cu121" "cu120" "cu118" "cu117" "cu116" "cu115" "cu114" "cu113" "cu112" "cu111" "cu110" "cu102" "cu101" "cu100" "cu92" "cu91" "cu90" "cu80" "rocm7.2" "rocm7.1" "rocm7.0" "rocm6.4" "rocm6.3" "rocm6.2.4" "rocm6.2" "rocm6.1" "rocm6.0" "rocm5.7" "rocm5.6" "rocm5.5" "rocm5.4.2" "rocm5.4" "rocm5.3" "rocm5.2" "rocm5.1.1" "rocm4.2" "rocm4.1" "rocm4.0.1" "xpu" ]
  }

  def "nu-complete uv pip compile resolver" [] {
    [ "backtracking" "legacy" ]
  }

  def "nu-complete uv pip compile python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip compile python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip compile color" [] {
    [ "auto" "always" "never" ]
  }

  # Compile a `requirements.in` file to a `requirements.txt` or `pylock.toml` file
  export extern "uv pip compile" [
    --constraints(-c): path   # Constrain versions using the given requirements files [env: `UV_CONSTRAINT`=]
    --overrides: path         # Override versions using the given requirements files [env: `UV_OVERRIDE`=]
    --excludes: path          # Exclude packages from resolution using the given requirements files [env: `UV_EXCLUDE`=]
    --build-constraints(-b): path # Constrain build dependencies using the given requirements files when building source distributions [env: `UV_BUILD_CONSTRAINT`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes
    --extra: string           # Include optional dependencies from the specified extra name; may be provided more than once
    --all-extras              # Include all optional dependencies
    --no-all-extras
    --group: string           # Install the specified dependency group from a `pyproject.toml`
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --index-strategy: string@"nu-complete uv pip compile index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv pip compile keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv pip compile resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv pip compile prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv pip compile fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv pip compile link_mode" # The method to use when installing packages from the global cache
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --no-deps                 # Ignore package dependencies, instead only add those packages explicitly listed on the command line to the resulting requirements file
    --deps
    --output-file(-o): path   # Write the compiled requirements to the given `requirements.txt` or `pylock.toml` file
    --format: string@"nu-complete uv pip compile format" # The format in which the resolution should be output
    --no-strip-extras         # Include extras in the output file
    --strip-extras
    --no-strip-markers        # Include environment markers in the output file
    --strip-markers
    --no-annotate             # Exclude comment annotations indicating the source of each package
    --annotate
    --no-header               # Exclude the comment header at the top of the generated output file
    --header
    --annotation-style: string@"nu-complete uv pip compile annotation_style" # The style of the annotation comments included in the output file, used to indicate the source of each package
    --custom-compile-command: string # The header comment to include at the top of the output file generated by `uv pip compile`
    --python(-p): string      # The Python interpreter to use during resolution.
    --system                  # Install packages into the system Python environment
    --no-system
    --generate-hashes         # Include distribution hashes in the output file
    --no-generate-hashes
    --no-build                # Don't build source distributions
    --build
    --no-binary: string       # Don't install pre-built wheels
    --only-binary: string     # Only use pre-built wheels; don't build source distributions
    --python-version: string  # The Python version to use for resolution
    --python-platform: string@"nu-complete uv pip compile python_platform" # The platform for which requirements should be resolved
    --universal               # Perform a universal resolution, attempting to generate a single `requirements.txt` output file that is compatible with all operating systems, architectures, and Python implementations
    --no-universal
    --no-emit-package: string # Specify a package to omit from the output resolution. Its dependencies will still be included in the resolution. Equivalent to pip-compile's `--unsafe-package` option
    --emit-index-url          # Include `--index-url` and `--extra-index-url` entries in the generated output file
    --no-emit-index-url
    --emit-find-links         # Include `--find-links` entries in the generated output file
    --no-emit-find-links
    --emit-build-options      # Include `--no-binary` and `--only-binary` entries in the generated output file
    --no-emit-build-options
    --emit-marker-expression  # Whether to emit a marker string indicating when it is known that the resulting set of pinned dependencies is valid
    --no-emit-marker-expression
    --emit-index-annotation   # Include comment annotations indicating the index used to resolve each package (e.g., `# from https://pypi.org/simple`)
    --no-emit-index-annotation
    --torch-backend: string@"nu-complete uv pip compile torch_backend" # The backend to use when fetching packages in the PyTorch ecosystem (e.g., `cpu`, `cu126`, or `auto`)
    --allow-unsafe
    --no-allow-unsafe
    --reuse-hashes
    --no-reuse-hashes
    --resolver: string@"nu-complete uv pip compile resolver"
    --max-rounds: string
    --client-cert: string
    --emit-trusted-host
    --no-emit-trusted-host
    --config: string
    --no-config
    --emit-options
    --no-emit-options
    --pip-args: string
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip compile python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip compile python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip compile color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --help(-h)                # Display the concise help for this command
    ...src_file: path         # Include the packages listed in the given files
  ]

  def "nu-complete uv pip sync index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv pip sync keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv pip sync link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv pip sync python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv pip sync output_format" [] {
    [ "text" "json" ]
  }

  def "nu-complete uv pip sync torch_backend" [] {
    [ "auto" "cpu" "cu132" "cu130" "cu129" "cu128" "cu126" "cu125" "cu124" "cu123" "cu122" "cu121" "cu120" "cu118" "cu117" "cu116" "cu115" "cu114" "cu113" "cu112" "cu111" "cu110" "cu102" "cu101" "cu100" "cu92" "cu91" "cu90" "cu80" "rocm7.2" "rocm7.1" "rocm7.0" "rocm6.4" "rocm6.3" "rocm6.2.4" "rocm6.2" "rocm6.1" "rocm6.0" "rocm5.7" "rocm5.6" "rocm5.5" "rocm5.4.2" "rocm5.4" "rocm5.3" "rocm5.2" "rocm5.1.1" "rocm4.2" "rocm4.1" "rocm4.0.1" "xpu" ]
  }

  def "nu-complete uv pip sync python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip sync python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip sync color" [] {
    [ "auto" "always" "never" ]
  }

  # Sync an environment with a `requirements.txt` or `pylock.toml` file
  export extern "uv pip sync" [
    --constraints(-c): path   # Constrain versions using the given requirements files [env: `UV_CONSTRAINT`=]
    --build-constraints(-b): path # Constrain build dependencies using the given requirements files when building source distributions [env: `UV_BUILD_CONSTRAINT`=]
    --extra: string           # Include optional dependencies from the specified extra name; may be provided more than once
    --all-extras              # Include all optional dependencies
    --no-all-extras
    --group: string           # Install the specified dependency group from a `pylock.toml` or `pyproject.toml`
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv pip sync index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv pip sync keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv pip sync link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --require-hashes          # Require a matching hash for each requirement
    --no-require-hashes
    --verify-hashes
    --no-verify-hashes        # Disable validation of hashes in the requirements file
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes
    --python(-p): string      # The Python interpreter into which packages should be installed.
    --system                  # Install packages into the system Python environment
    --no-system
    --break-system-packages   # Allow uv to modify an `EXTERNALLY-MANAGED` Python installation
    --no-break-system-packages
    --target(-t): path        # Install packages into the specified directory, rather than into the virtual or system Python environment. The packages will be installed at the top-level of the directory
    --prefix: path            # Install packages into `lib`, `bin`, and other top-level folders under the specified directory, as if a virtual environment were present at that location
    --no-build                # Don't build source distributions
    --build
    --no-binary: string       # Don't install pre-built wheels
    --only-binary: string     # Only use pre-built wheels; don't build source distributions
    --allow-empty-requirements # Allow sync of empty requirements, which will clear the environment of all packages
    --no-allow-empty-requirements
    --python-version: string  # The minimum Python version that should be supported by the requirements (e.g., `3.7` or `3.7.9`)
    --python-platform: string@"nu-complete uv pip sync python_platform" # The platform for which requirements should be installed
    --strict                  # Validate the Python environment after completing the installation, to detect packages with missing dependencies or other issues
    --no-strict
    --dry-run                 # Perform a dry run, i.e., don't actually install anything but resolve the dependencies and print the resulting plan
    --check                   # Check whether the environment matches the requirements without modifying it
    --output-format: string@"nu-complete uv pip sync output_format" # Select the output format
    --torch-backend: string@"nu-complete uv pip sync torch_backend" # The backend to use when fetching packages in the PyTorch ecosystem (e.g., `cpu`, `cu126`, or `auto`)
    --ask(-a)
    --python-executable: string
    --user
    --client-cert: string
    --config: string
    --no-config
    --pip-args: string
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip sync python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip sync python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip sync color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --help(-h)                # Display the concise help for this command
    ...src_file: path         # Include the packages listed in the given files
  ]

  def "nu-complete uv pip install index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv pip install keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv pip install resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv pip install prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv pip install fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv pip install link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv pip install python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv pip install output_format" [] {
    [ "text" "json" ]
  }

  def "nu-complete uv pip install torch_backend" [] {
    [ "auto" "cpu" "cu132" "cu130" "cu129" "cu128" "cu126" "cu125" "cu124" "cu123" "cu122" "cu121" "cu120" "cu118" "cu117" "cu116" "cu115" "cu114" "cu113" "cu112" "cu111" "cu110" "cu102" "cu101" "cu100" "cu92" "cu91" "cu90" "cu80" "rocm7.2" "rocm7.1" "rocm7.0" "rocm6.4" "rocm6.3" "rocm6.2.4" "rocm6.2" "rocm6.1" "rocm6.0" "rocm5.7" "rocm5.6" "rocm5.5" "rocm5.4.2" "rocm5.4" "rocm5.3" "rocm5.2" "rocm5.1.1" "rocm4.2" "rocm4.1" "rocm4.0.1" "xpu" ]
  }

  def "nu-complete uv pip install python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip install python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip install color" [] {
    [ "auto" "always" "never" ]
  }

  # Install packages into an environment
  export extern "uv pip install" [
    --requirements(-r): path  # Install the packages listed in the given files
    --editable(-e): string    # Install the editable package based on the provided local file path
    --no-editable             # Install any editable dependencies as non-editable [env: UV_NO_EDITABLE=]
    --no-editable-package: string # Install the specified editable packages as non-editable
    --constraints(-c): path   # Constrain versions using the given requirements files [env: `UV_CONSTRAINT`=]
    --overrides: path         # Override versions using the given requirements files [env: `UV_OVERRIDE`=]
    --excludes: path          # Exclude packages from resolution using the given requirements files [env: `UV_EXCLUDE`=]
    --build-constraints(-b): path # Constrain build dependencies using the given requirements files when building source distributions [env: `UV_BUILD_CONSTRAINT`=]
    --extra: string           # Include optional dependencies from the specified extra name; may be provided more than once
    --all-extras              # Include all optional dependencies
    --no-all-extras
    --group: string           # Install the specified dependency group from a `pylock.toml` or `pyproject.toml`
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --reinstall               # Reinstall all packages, regardless of whether they're already installed. Implies `--refresh`
    --no-reinstall
    --reinstall-package: string # Reinstall a specific package, regardless of whether it's already installed. Implies `--refresh-package`
    --index-strategy: string@"nu-complete uv pip install index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv pip install keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv pip install resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv pip install prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv pip install fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv pip install link_mode" # The method to use when installing packages from the global cache
    --compile-bytecode        # Compile Python files to bytecode after installation
    --no-compile-bytecode
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --no-deps                 # Ignore package dependencies, instead only installing those packages explicitly listed on the command line or in the requirements files
    --deps
    --require-hashes          # Require a matching hash for each requirement
    --no-require-hashes
    --verify-hashes
    --no-verify-hashes        # Disable validation of hashes in the requirements file
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes
    --python(-p): string      # The Python interpreter into which packages should be installed.
    --system                  # Install packages into the system Python environment
    --no-system
    --break-system-packages   # Allow uv to modify an `EXTERNALLY-MANAGED` Python installation
    --no-break-system-packages
    --target(-t): path        # Install packages into the specified directory, rather than into the virtual or system Python environment. The packages will be installed at the top-level of the directory
    --prefix: path            # Install packages into `lib`, `bin`, and other top-level folders under the specified directory, as if a virtual environment were present at that location
    --no-build                # Don't build source distributions
    --build
    --no-binary: string       # Don't install pre-built wheels
    --only-binary: string     # Only use pre-built wheels; don't build source distributions
    --python-version: string  # The minimum Python version that should be supported by the requirements (e.g., `3.7` or `3.7.9`)
    --python-platform: string@"nu-complete uv pip install python_platform" # The platform for which requirements should be installed
    --inexact                 # Do not remove extraneous packages present in the environment
    --exact                   # Perform an exact sync, removing extraneous packages
    --strict                  # Validate the Python environment after completing the installation, to detect packages with missing dependencies or other issues
    --no-strict
    --dry-run                 # Perform a dry run, i.e., don't actually install anything but resolve the dependencies and print the resulting plan
    --check                   # Check whether the environment satisfies the requirements without modifying it
    --output-format: string@"nu-complete uv pip install output_format" # Select the output format
    --torch-backend: string@"nu-complete uv pip install torch_backend" # The backend to use when fetching packages in the PyTorch ecosystem (e.g., `cpu`, `cu126`, or `auto`)
    --disable-pip-version-check
    --user
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip install python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip install python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip install color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...package: string        # Install all listed packages
  ]

  def "nu-complete uv pip uninstall keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv pip uninstall python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip uninstall python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip uninstall color" [] {
    [ "auto" "always" "never" ]
  }

  # Uninstall packages from an environment
  export extern "uv pip uninstall" [
    --requirements(-r): path  # Uninstall the packages listed in the given files
    --python(-p): string      # The Python interpreter from which packages should be uninstalled.
    --keyring-provider: string@"nu-complete uv pip uninstall keyring_provider" # Attempt to use `keyring` for authentication for remote requirements files
    --system                  # Use the system Python to uninstall packages
    --no-system
    --break-system-packages   # Allow uv to modify an `EXTERNALLY-MANAGED` Python installation
    --no-break-system-packages
    --target(-t): path        # Uninstall packages from the specified `--target` directory
    --prefix: path            # Uninstall packages from the specified `--prefix` directory
    --dry-run                 # Perform a dry run, i.e., don't actually uninstall anything but print the resulting plan
    --yes(-y)                 # Don't ask for confirmation of uninstall deletions
    --disable-pip-version-check
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip uninstall python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip uninstall python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip uninstall color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...package: string        # Uninstall all listed packages
  ]

  def "nu-complete uv pip freeze python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip freeze python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip freeze color" [] {
    [ "auto" "always" "never" ]
  }

  # List, in requirements format, packages installed in an environment
  export extern "uv pip freeze" [
    --exclude-editable        # Exclude any editable packages from output
    --exclude: string         # Exclude the specified package(s) from the output
    --strict                  # Validate the Python environment, to detect packages with missing dependencies and other issues
    --no-strict
    --python(-p): string      # The Python interpreter for which packages should be listed.
    --path: path              # Restrict to the specified installation path for listing packages (can be used multiple times)
    --system                  # List packages in the system Python environment
    --no-system
    --target(-t): path        # List packages from the specified `--target` directory
    --prefix: path            # List packages from the specified `--prefix` directory
    --disable-pip-version-check
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip freeze python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip freeze python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip freeze color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv pip list format" [] {
    [ "columns" "freeze" "json" ]
  }

  def "nu-complete uv pip list index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv pip list keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv pip list python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip list python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip list color" [] {
    [ "auto" "always" "never" ]
  }

  # List, in tabular format, packages installed in an environment
  export extern "uv pip list" [
    --editable(-e)            # Only include editable projects
    --exclude-editable        # Exclude any editable packages from output
    --exclude: string         # Exclude the specified package(s) from the output
    --format: string@"nu-complete uv pip list format" # Select the output format
    --outdated                # List outdated packages
    --no-outdated
    --strict                  # Validate the Python environment, to detect packages with missing dependencies and other issues
    --no-strict
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --index-strategy: string@"nu-complete uv pip list index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv pip list keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --python(-p): string      # The Python interpreter for which packages should be listed.
    --system                  # List packages in the system Python environment
    --no-system
    --target(-t): path        # List packages from the specified `--target` directory
    --prefix: path            # List packages from the specified `--prefix` directory
    --disable-pip-version-check
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip list python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip list python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip list color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv pip show python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip show python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip show color" [] {
    [ "auto" "always" "never" ]
  }

  # Show information about one or more installed packages
  export extern "uv pip show" [
    --strict                  # Validate the Python environment, to detect packages with missing dependencies and other issues
    --no-strict
    --files(-f)               # Show the full list of installed files for each package
    --python(-p): string      # The Python interpreter to find the package in.
    --system                  # Show a package in the system Python environment
    --no-system
    --target(-t): path        # Show a package from the specified `--target` directory
    --prefix: path            # Show a package from the specified `--prefix` directory
    --disable-pip-version-check
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip show python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip show python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip show color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...package: string        # The package(s) to display
  ]

  def "nu-complete uv pip tree index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv pip tree keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv pip tree python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip tree python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip tree color" [] {
    [ "auto" "always" "never" ]
  }

  # Display the dependency tree for an environment
  export extern "uv pip tree" [
    --show-version-specifiers # Show the version constraint(s) imposed on each package
    --depth(-d): string       # Maximum display depth of the dependency tree
    --prune: string           # Prune the given package from the display of the dependency tree
    --package: string         # Display only the specified packages
    --no-dedupe               # Do not de-duplicate repeated dependencies. Usually, when a package has already displayed its dependencies, further occurrences will not re-display its dependencies, and will include a (*) to indicate it has already been shown. This flag will cause those duplicates to be repeated
    --invert                  # Show the reverse dependencies for the given package. This flag will invert the tree and display the packages that depend on the given package
    --outdated                # Show the latest available version of each package in the tree
    --show-sizes              # Show compressed wheel sizes for packages in the tree
    --strict                  # Validate the Python environment, to detect packages with missing dependencies and other issues
    --no-strict
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --index-strategy: string@"nu-complete uv pip tree index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv pip tree keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --python(-p): string      # The Python interpreter for which packages should be listed.
    --system                  # List packages in the system Python environment
    --no-system
    --disable-pip-version-check
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip tree python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip tree python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip tree color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv pip check python_platform" [] {
    [ "windows" "linux" "macos" "x86_64-pc-windows-msvc" "aarch64-pc-windows-msvc" "i686-pc-windows-msvc" "x86_64-unknown-linux-gnu" "aarch64-apple-darwin" "x86_64-apple-darwin" "aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl" "x86_64-unknown-linux-musl" "s390x-unknown-linux-gnu" "powerpc64le-unknown-linux-gnu" "loongarch64-unknown-linux-gnu" "riscv64-unknown-linux" "x86_64-manylinux2014" "x86_64-manylinux_2_17" "x86_64-manylinux_2_28" "x86_64-manylinux_2_31" "x86_64-manylinux_2_32" "x86_64-manylinux_2_33" "x86_64-manylinux_2_34" "x86_64-manylinux_2_35" "x86_64-manylinux_2_36" "x86_64-manylinux_2_37" "x86_64-manylinux_2_38" "x86_64-manylinux_2_39" "x86_64-manylinux_2_40" "aarch64-manylinux2014" "aarch64-manylinux_2_17" "aarch64-manylinux_2_28" "aarch64-manylinux_2_31" "aarch64-manylinux_2_32" "aarch64-manylinux_2_33" "aarch64-manylinux_2_34" "aarch64-manylinux_2_35" "aarch64-manylinux_2_36" "aarch64-manylinux_2_37" "aarch64-manylinux_2_38" "aarch64-manylinux_2_39" "aarch64-manylinux_2_40" "s390x-manylinux2014" "s390x-manylinux_2_17" "s390x-manylinux_2_28" "s390x-manylinux_2_31" "s390x-manylinux_2_32" "s390x-manylinux_2_33" "s390x-manylinux_2_34" "s390x-manylinux_2_35" "s390x-manylinux_2_36" "s390x-manylinux_2_37" "s390x-manylinux_2_38" "s390x-manylinux_2_39" "s390x-manylinux_2_40" "ppc64le-manylinux2014" "ppc64le-manylinux_2_17" "ppc64le-manylinux_2_28" "ppc64le-manylinux_2_31" "ppc64le-manylinux_2_32" "ppc64le-manylinux_2_33" "ppc64le-manylinux_2_34" "ppc64le-manylinux_2_35" "ppc64le-manylinux_2_36" "ppc64le-manylinux_2_37" "ppc64le-manylinux_2_38" "ppc64le-manylinux_2_39" "ppc64le-manylinux_2_40" "loongarch64-manylinux_2_36" "loongarch64-manylinux_2_37" "loongarch64-manylinux_2_38" "loongarch64-manylinux_2_39" "loongarch64-manylinux_2_40" "aarch64-linux-android" "x86_64-linux-android" "wasm32-pyodide2024" "wasm32-pyodide2025" "arm64-apple-ios" "arm64-apple-ios-simulator" "x86_64-apple-ios-simulator" ]
  }

  def "nu-complete uv pip check python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip check python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip check color" [] {
    [ "auto" "always" "never" ]
  }

  # Verify installed packages have compatible dependencies
  export extern "uv pip check" [
    --python(-p): string      # The Python interpreter for which packages should be checked.
    --system                  # Check packages in the system Python environment
    --no-system
    --python-version: string  # The Python version against which packages should be checked
    --python-platform: string@"nu-complete uv pip check python_platform" # The platform for which packages should be checked
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip check python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip check python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip check color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv pip debug python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv pip debug python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv pip debug color" [] {
    [ "auto" "always" "never" ]
  }

  # Display debug information (unsupported)
  export extern "uv pip debug" [
    --platform: string
    --python-version: string
    --implementation: string
    --abi: string
    --cert: path              # Path to a PEM-encoded CA certificate bundle
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv pip debug python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv pip debug python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv pip debug color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv venv index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv venv keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv venv link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv venv python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv venv python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv venv color" [] {
    [ "auto" "always" "never" ]
  }

  # Create a virtual environment
  export extern "uv venv" [
    --python(-p): string      # The Python interpreter to use for the virtual environment.
    --system                  # Ignore virtual environments when searching for the Python interpreter
    --no-system               # This flag is included for compatibility only, it has no effect
    --no-project              # Avoid discovering a project or workspace
    --seed                    # Install seed packages (one or more of: `pip`, `setuptools`, and `wheel`) into the virtual environment [env: UV_VENV_SEED=]
    --clear(-c)               # Remove any existing files or directories at the target path [env: UV_VENV_CLEAR=]
    --force                   # Allow `--clear` to remove a non-virtual environment directory
    --no-clear                # Fail without prompting if any existing files or directories are present at the target path
    --allow-existing          # Preserve any existing files or directories at the target path
    --prompt: string          # Provide an alternative prompt prefix for the virtual environment.
    --system-site-packages    # Give the virtual environment access to the system site packages directory
    --relocatable             # Make the virtual environment relocatable [env: UV_VENV_RELOCATABLE=]
    --no-relocatable          # Don't make the virtual environment relocatable
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --index-strategy: string@"nu-complete uv venv index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv venv keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv venv link_mode" # The method to use when installing packages from the global cache
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --no-seed
    --no-pip
    --no-setuptools
    --no-wheel
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv venv python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv venv python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv venv color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    path?: path               # The path to the virtual environment to create
  ]

  def "nu-complete uv build index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv build keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv build resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv build prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv build fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv build link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv build python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build color" [] {
    [ "auto" "always" "never" ]
  }

  # Build Python packages into source distributions and wheels
  export extern "uv build" [
    --skip-dependency-check   # Skip checking if build dependencies are satisfied when building without isolation
    --package: string         # Build a specific package in the workspace
    --all-packages            # Builds all packages in the workspace
    --out-dir(-o): path       # The output directory to which distributions should be written
    --sdist                   # Build a source distribution ("sdist") from the given directory
    --wheel                   # Build a binary distribution ("wheel") from the given directory
    --list                    # When using the uv build backend, list the files that would be included when building
    --build-logs
    --no-build-logs           # Hide logs from the build backend
    --force-pep517            # Always build through PEP 517, don't use the fast path for the uv build backend
    --clear                   # Clear the output directory before the build, removing stale artifacts
    --create-gitignore
    --no-create-gitignore     # Do not create a `.gitignore` file in the output directory
    --build-constraints(-b): path # Constrain build dependencies using the given requirements files when building distributions [env: `UV_BUILD_CONSTRAINT`=]
    --require-hashes          # Require a matching hash for each requirement
    --no-require-hashes
    --verify-hashes
    --no-verify-hashes        # Disable validation of hashes in the requirements file
    --python(-p): string      # The Python interpreter to use for the build environment.
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --index-strategy: string@"nu-complete uv build index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv build keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv build resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv build prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv build fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv build link_mode" # The method to use when installing packages from the global cache
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    src?: path                # The directory from which distributions should be built, or a source distribution archive to build into a wheel
  ]

  def "nu-complete uv publish trusted_publishing" [] {
    [ "automatic" "always" "never" ]
  }

  def "nu-complete uv publish keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv publish python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv publish python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv publish color" [] {
    [ "auto" "always" "never" ]
  }

  # Upload distributions to an index
  export extern "uv publish" [
    --offline
    --index: string           # The name of an index in the configuration to use for publishing.
    --username(-u): string    # The username for the upload
    --password(-p): string    # The password for the upload
    --token(-t): string       # The token for the upload
    --trusted-publishing: string@"nu-complete uv publish trusted_publishing" # Configure trusted publishing
    --keyring-provider: string@"nu-complete uv publish keyring_provider" # Attempt to use `keyring` for authentication for remote requirements files
    --publish-url: string     # The URL of the upload endpoint (not the index URL)
    --check-url: string       # Check an index URL for existing files to skip duplicate uploads
    --skip-existing
    --dry-run                 # Perform a dry run without uploading files
    --no-attestations         # Do not upload attestations for the published files
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv publish python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv publish python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv publish color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...files: path            # Paths to the files to upload. Accepts glob expressions
  ]

  def "nu-complete uv workspace python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv workspace python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv workspace color" [] {
    [ "auto" "always" "never" ]
  }

  # Inspect uv workspaces
  export extern "uv workspace" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv workspace python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv workspace python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv workspace color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv workspace metadata index_strategy" [] {
    [ "first-index" "unsafe-first-match" "unsafe-best-match" ]
  }

  def "nu-complete uv workspace metadata keyring_provider" [] {
    [ "disabled" "subprocess" ]
  }

  def "nu-complete uv workspace metadata resolution" [] {
    [ "highest" "lowest" "lowest-direct" ]
  }

  def "nu-complete uv workspace metadata prerelease" [] {
    [ "disallow" "allow" "if-necessary" "explicit" "if-necessary-or-explicit" ]
  }

  def "nu-complete uv workspace metadata fork_strategy" [] {
    [ "fewest" "requires-python" ]
  }

  def "nu-complete uv workspace metadata link_mode" [] {
    [ "clone" "copy" "hardlink" "symlink" ]
  }

  def "nu-complete uv workspace metadata python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv workspace metadata python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv workspace metadata color" [] {
    [ "auto" "always" "never" ]
  }

  # View metadata about the current workspace
  export extern "uv workspace metadata" [
    --script: path            # View metadata for the specified PEP 723 Python script, rather than the current workspace
    --locked                  # Check if the lockfile is up-to-date [env: UV_LOCKED=]
    --no-locked               # Disable locked mode, overriding `UV_LOCKED`
    --frozen                  # Assert that a `uv.lock` exists without checking if it is up-to-date [env: UV_FROZEN=]
    --no-frozen               # Disable frozen mode, overriding `UV_FROZEN`
    --index: string           # The indexes to use when resolving dependencies, in addition to the default index
    --default-index: string   # The default package index (by default: <https://pypi.org/simple>)
    --index-url(-i): string   # (Deprecated: use `--default-index` instead) The URL of the Python package index (by default: <https://pypi.org/simple>)
    --extra-index-url: string # (Deprecated: use `--index` instead) Extra URLs of package indexes to use, in addition to `--index-url`
    --find-links(-f): string  # Locations to search for candidate distributions, in addition to those found in the registry indexes
    --no-index                # Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those provided via `--find-links`
    --upgrade(-U)             # Allow package upgrades, ignoring pinned versions in any existing output file. Implies `--refresh`
    --no-upgrade
    --upgrade-package(-P): string # Allow upgrades for a specific package, ignoring pinned versions in any existing output file. Implies `--refresh-package`
    --upgrade-group: string   # Allow upgrades for all packages in a dependency group, ignoring pinned versions in any existing output file
    --index-strategy: string@"nu-complete uv workspace metadata index_strategy" # The strategy to use when resolving against multiple index URLs
    --keyring-provider: string@"nu-complete uv workspace metadata keyring_provider" # Attempt to use `keyring` for authentication for index URLs
    --resolution: string@"nu-complete uv workspace metadata resolution" # The strategy to use when selecting between the different compatible versions for a given package requirement
    --prerelease: string@"nu-complete uv workspace metadata prerelease" # The strategy to use when considering pre-release versions
    --prerelease-package: string # The strategy to use when considering pre-release versions for a specific package
    --pre
    --fork-strategy: string@"nu-complete uv workspace metadata fork_strategy" # The strategy to use when selecting multiple versions of a given package across Python versions and platforms
    --config-setting(-C): string # Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs
    --config-settings-package: string # Settings to pass to the PEP 517 build backend for a specific package, specified as `PACKAGE:KEY=VALUE` pairs
    --no-build-isolation      # Disable isolation when building source distributions
    --build-isolation
    --no-build-isolation-package: string # Disable isolation when building source distributions for a specific package
    --exclude-newer: string   # Limit candidate packages to those that were uploaded prior to the given date
    --exclude-newer-package: string # Limit candidate packages for specific packages to those that were uploaded prior to the given date
    --link-mode: string@"nu-complete uv workspace metadata link_mode" # The method to use when installing packages from the global cache
    --no-sources              # Ignore the `tool.uv.sources` table when resolving dependencies. Used to lock against the standards-compliant, publishable package metadata, as opposed to using any workspace, Git, URL, or local path sources
    --no-sources-package: string # Don't use sources from the `tool.uv.sources` table for the specified packages [env: `UV_NO_SOURCES_PACKAGE`=]
    --no-build                # Don't build source distributions
    --build
    --no-build-package: string # Don't build source distributions for a specific package [env: `UV_NO_BUILD_PACKAGE`=]
    --no-binary               # Don't install pre-built wheels
    --binary
    --no-binary-package: string # Don't install pre-built wheels for a specific package [env: `UV_NO_BINARY_PACKAGE`=]
    --require-build-hashes    # Require hashes for all build dependencies
    --no-require-build-hashes # Do not require hashes for every build dependency
    --refresh                 # Refresh all cached data
    --no-refresh
    --refresh-package: string # Refresh cached data for a specific package
    --sync                    # Sync the environment to include module ownership metadata in the output
    --exact                   # Perform an exact sync, removing extraneous packages
    --active                  # Sync dependencies to the active virtual environment
    --python(-p): string      # The Python interpreter to use during resolution.
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv workspace metadata python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv workspace metadata python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv workspace metadata color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv workspace dir python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv workspace dir python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv workspace dir color" [] {
    [ "auto" "always" "never" ]
  }

  # Display the path of a workspace member
  export extern "uv workspace dir" [
    --package: string         # Display the path to a specific package in the workspace
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv workspace dir python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv workspace dir python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv workspace dir color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv workspace list python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv workspace list python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv workspace list color" [] {
    [ "auto" "always" "never" ]
  }

  # List the members of a workspace
  export extern "uv workspace list" [
    --paths                   # Show paths instead of names
    --scripts                 # List all standalone scripts with inline metadata in the workspace
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv workspace list python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv workspace list python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv workspace list color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv build-backend python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build-backend python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build-backend color" [] {
    [ "auto" "always" "never" ]
  }

  # The implementation of the build backend
  export extern "uv build-backend" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build-backend python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build-backend python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build-backend color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv build-backend build-sdist python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build-backend build-sdist python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build-backend build-sdist color" [] {
    [ "auto" "always" "never" ]
  }

  # PEP 517 hook `build_sdist`
  export extern "uv build-backend build-sdist" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build-backend build-sdist python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build-backend build-sdist python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build-backend build-sdist color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    sdist_directory: path
  ]

  def "nu-complete uv build-backend build-wheel python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build-backend build-wheel python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build-backend build-wheel color" [] {
    [ "auto" "always" "never" ]
  }

  # PEP 517 hook `build_wheel`
  export extern "uv build-backend build-wheel" [
    --metadata-directory: path
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build-backend build-wheel python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build-backend build-wheel python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build-backend build-wheel color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    wheel_directory: path
  ]

  def "nu-complete uv build-backend build-editable python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build-backend build-editable python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build-backend build-editable color" [] {
    [ "auto" "always" "never" ]
  }

  # PEP 660 hook `build_editable`
  export extern "uv build-backend build-editable" [
    --metadata-directory: path
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build-backend build-editable python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build-backend build-editable python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build-backend build-editable color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    wheel_directory: path
  ]

  def "nu-complete uv build-backend get-requires-for-build-sdist python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build-backend get-requires-for-build-sdist python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build-backend get-requires-for-build-sdist color" [] {
    [ "auto" "always" "never" ]
  }

  # PEP 517 hook `get_requires_for_build_sdist`
  export extern "uv build-backend get-requires-for-build-sdist" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build-backend get-requires-for-build-sdist python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build-backend get-requires-for-build-sdist python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build-backend get-requires-for-build-sdist color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv build-backend get-requires-for-build-wheel python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build-backend get-requires-for-build-wheel python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build-backend get-requires-for-build-wheel color" [] {
    [ "auto" "always" "never" ]
  }

  # PEP 517 hook `get_requires_for_build_wheel`
  export extern "uv build-backend get-requires-for-build-wheel" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build-backend get-requires-for-build-wheel python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build-backend get-requires-for-build-wheel python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build-backend get-requires-for-build-wheel color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv build-backend prepare-metadata-for-build-wheel python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build-backend prepare-metadata-for-build-wheel python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build-backend prepare-metadata-for-build-wheel color" [] {
    [ "auto" "always" "never" ]
  }

  # PEP 517 hook `prepare_metadata_for_build_wheel`
  export extern "uv build-backend prepare-metadata-for-build-wheel" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build-backend prepare-metadata-for-build-wheel python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build-backend prepare-metadata-for-build-wheel python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build-backend prepare-metadata-for-build-wheel color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    wheel_directory: path
  ]

  def "nu-complete uv build-backend get-requires-for-build-editable python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build-backend get-requires-for-build-editable python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build-backend get-requires-for-build-editable color" [] {
    [ "auto" "always" "never" ]
  }

  # PEP 660 hook `get_requires_for_build_editable`
  export extern "uv build-backend get-requires-for-build-editable" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build-backend get-requires-for-build-editable python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build-backend get-requires-for-build-editable python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build-backend get-requires-for-build-editable color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv build-backend prepare-metadata-for-build-editable python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv build-backend prepare-metadata-for-build-editable python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv build-backend prepare-metadata-for-build-editable color" [] {
    [ "auto" "always" "never" ]
  }

  # PEP 660 hook `prepare_metadata_for_build_editable`
  export extern "uv build-backend prepare-metadata-for-build-editable" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv build-backend prepare-metadata-for-build-editable python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv build-backend prepare-metadata-for-build-editable python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv build-backend prepare-metadata-for-build-editable color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    wheel_directory: path
  ]

  def "nu-complete uv cache python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv cache python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv cache color" [] {
    [ "auto" "always" "never" ]
  }

  # Manage uv's cache
  export extern "uv cache" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv cache python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv cache python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv cache color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv cache clean python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv cache clean python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv cache clean color" [] {
    [ "auto" "always" "never" ]
  }

  # Clear the cache, removing all entries or those linked to specific packages
  export extern "uv cache clean" [
    --force                   # Force removal of the cache, ignoring in-use checks
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv cache clean python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv cache clean python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv cache clean color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...package: string        # The packages to remove from the cache
  ]

  def "nu-complete uv cache prune python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv cache prune python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv cache prune color" [] {
    [ "auto" "always" "never" ]
  }

  # Prune dangling cache entries and cached environments
  export extern "uv cache prune" [
    --ci                      # Optimize the cache for persistence in a continuous integration environment, like GitHub Actions
    --force                   # Force removal of the cache, ignoring in-use checks
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv cache prune python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv cache prune python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv cache prune color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv cache dir python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv cache dir python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv cache dir color" [] {
    [ "auto" "always" "never" ]
  }

  # Show the cache directory
  export extern "uv cache dir" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv cache dir python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv cache dir python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv cache dir color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv cache size output_format" [] {
    [ "auto" "human" "machine" ]
  }

  def "nu-complete uv cache size python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv cache size python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv cache size color" [] {
    [ "auto" "always" "never" ]
  }

  # Show the cache size
  export extern "uv cache size" [
    --output-format: string@"nu-complete uv cache size output_format" # Select the output format
    --human(-H)               # Display the cache size in human-readable format (e.g., `1.2GiB` instead of raw bytes)
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv cache size python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv cache size python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv cache size color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv self python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv self python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv self color" [] {
    [ "auto" "always" "never" ]
  }

  # Manage the uv executable
  export extern "uv self" [
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv self python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv self python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv self color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv self update python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv self update python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv self update color" [] {
    [ "auto" "always" "never" ]
  }

  # Update uv
  export extern "uv self update" [
    --token: string           # A GitHub token for authentication. A token is not required but can be used to reduce the chance of encountering rate limits
    --dry-run                 # Run without performing the update
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv self update python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv self update python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv self update color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    target_version?: string   # Update to the specified version. If not provided, uv will update to the latest version
  ]

  def "nu-complete uv self version output_format" [] {
    [ "text" "json" ]
  }

  def "nu-complete uv self version python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv self version python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv self version color" [] {
    [ "auto" "always" "never" ]
  }

  # Display uv's version
  export extern "uv self version" [
    --short                   # Only print the version
    --output-format: string@"nu-complete uv self version output_format"
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv self version python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv self version python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv self version color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
  ]

  def "nu-complete uv clean python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv clean python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv clean color" [] {
    [ "auto" "always" "never" ]
  }

  # Clear the cache, removing all entries or those linked to specific packages
  export extern "uv clean" [
    --force                   # Force removal of the cache, ignoring in-use checks
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv clean python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv clean python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv clean color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...package: string        # The packages to remove from the cache
  ]

  def "nu-complete uv generate-shell-completion shell" [] {
    [ "bash" "elvish" "fish" "nushell" "powershell" "zsh" ]
  }

  def "nu-complete uv generate-shell-completion python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv generate-shell-completion color" [] {
    [ "auto" "always" "never" ]
  }

  def "nu-complete uv generate-shell-completion python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  # Generate shell completion
  export extern "uv generate-shell-completion" [
    --no-cache(-n)
    --cache-dir: path
    --python-preference: string@"nu-complete uv generate-shell-completion python_preference"
    --no-python-downloads
    --quiet(-q)
    --verbose(-v)
    --color: string@"nu-complete uv generate-shell-completion color"
    --native-tls
    --offline
    --no-progress
    --config-file: path
    --no-config
    --help(-h)
    --version(-V)
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --python-fetch: string@"nu-complete uv generate-shell-completion python_fetch" # Deprecated option for configuring automatic Python downloads
    --no-color                # Disable colors
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    shell: string@"nu-complete uv generate-shell-completion shell" # The shell to generate the completion script for
  ]

  def "nu-complete uv help python_preference" [] {
    [ "only-managed" "managed" "system" "only-system" ]
  }

  def "nu-complete uv help python_fetch" [] {
    [ "automatic" "manual" "never" ]
  }

  def "nu-complete uv help color" [] {
    [ "auto" "always" "never" ]
  }

  # Display documentation for a command
  export extern "uv help" [
    --no-pager                # Disable pager when printing help
    --no-cache(-n)            # Avoid reading from or writing to the cache, instead using a temporary directory for the duration of the operation
    --cache-dir: path         # Path to the cache directory
    --python-preference: string@"nu-complete uv help python_preference"
    --managed-python          # Require use of uv-managed Python versions [env: UV_MANAGED_PYTHON=]
    --no-managed-python       # Disable use of uv-managed Python versions [env: UV_NO_MANAGED_PYTHON=]
    --allow-python-downloads  # Allow automatically downloading Python when required. [env: "UV_PYTHON_DOWNLOADS=auto"]
    --no-python-downloads     # Disable automatic downloads of Python. [env: "UV_PYTHON_DOWNLOADS=never"]
    --python-fetch: string@"nu-complete uv help python_fetch" # Deprecated option for configuring automatic Python downloads
    --quiet(-q)               # Use quiet output
    --verbose(-v)             # Use verbose output
    --no-color                # Disable colors
    --color: string@"nu-complete uv help color" # Control the use of color in output
    --native-tls              # (Deprecated: use `--system-certs` instead.) Whether to load TLS certificates from the platform's native certificate store [env: UV_NATIVE_TLS=]
    --no-native-tls
    --system-certs            # Whether to load TLS certificates from the platform's native certificate store [env: UV_SYSTEM_CERTS=]
    --no-system-certs
    --offline                 # Disable network access [env: UV_OFFLINE=]
    --no-offline
    --allow-insecure-host: string # Allow insecure connections to a host
    --preview                 # Whether to enable all experimental preview features [env: UV_PREVIEW=]
    --no-preview
    --preview-features: string # Enable experimental preview features
    --isolated                # Avoid discovering a `pyproject.toml` or `uv.toml` file [env: UV_ISOLATED=]
    --show-settings           # Show the resolved settings for the current command
    --no-progress             # Hide all progress outputs [env: UV_NO_PROGRESS=]
    --no-installer-metadata   # Skip writing `uv` installer metadata files (e.g., `INSTALLER`, `REQUESTED`, and `direct_url.json`) to site-packages `.dist-info` directories [env: UV_NO_INSTALLER_METADATA=]
    --directory: path         # Change to the given directory prior to running the command
    --project: path           # Discover a project in the given directory
    --config-file: path       # The path to a `uv.toml` file to use for configuration
    --no-config               # Avoid discovering configuration files (`pyproject.toml`, `uv.toml`)
    --help(-h)                # Display the concise help for this command
    ...command: string
  ]

}

export use completions *
