#include "pch.h"
#include "framework.h"
#include "Directory.hpp"
#include <filesystem>
#include <string>
#include <algorithm>

namespace Puma {
namespace File
{
	namespace Directory
	{
		// Get current working directory - normalized to forward slashes
		Types::String GetCurrentDirectory() noexcept
		{
			std::error_code ec;
			const std::filesystem::path path = std::filesystem::current_path(ec);
			// On error, return empty string
			if (ec)
			{
				return Types::String();
			}

			// Normalize to forward slashes - Puma does not support backslashes
			std::string normalizedPath = path.string();
			std::replace(normalizedPath.begin(), normalizedPath.end(), '\\', '/');
			// Return as Types::String
			return Types::String(normalizedPath.c_str());
		}

		// Set current working directory - normalized to forward slashes
		static bool SetCurrentDirectory(const Types::String& path) noexcept
		{
			const std::uint32_t pathSize = path.StrSize();
			// Empty path is invalid
			if (pathSize == 0)
			{
				return false;
			}
			// All modern day OSes support forward slashes, so normalize for portable code
			std::string normalizedPath(path.Data(), path.Data() + pathSize);
			std::replace(normalizedPath.begin(), normalizedPath.end(), '\\', '/');
			// Set current path
			std::error_code ec;
			std::filesystem::current_path(normalizedPath, ec);
			// Return success status
			return !ec;
		}
	} // namespace Directory
} // namespace File
} // namespace Puma