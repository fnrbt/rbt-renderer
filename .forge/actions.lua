-- Forge allows one publish entry per registry and ecosystem in an action, so
-- each package is published by its own action.
local function package(id)
    return function(_)
        return {
            output = "package",
            base = "set -euo pipefail; python3 .forge/publish " .. id,
            packages = {
                -- Read access makes the sandbox write a NuGet.Config that goes
                -- through its credentialed gateway. Without it the restore
                -- queries the registry anonymously and fails with 401.
                read = {
                    { registry = "41021897", ecosystem = "nuget" },
                },
                publish = {
                    { registry = "41021897", ecosystem = "nuget", mode = "explicit", prefix = id },
                },
            },
        }
    end
end

return {
    select = function(ctx)
        if string.match(ctx.ref_name, "^refs/tags/v%d+%.%d+%.%d+") then
            return { "publish-core", "publish" }
        end
        return {}
    end,
    ["publish-core"] = package("Rbt.Renderer.Core"),
    ["publish"] = package("Rbt.Renderer"),
}
